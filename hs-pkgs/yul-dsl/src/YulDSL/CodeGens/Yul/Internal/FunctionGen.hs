{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module YulDSL.CodeGens.Yul.Internal.FunctionGen
  ( compile_cat
  , compile_fn
  , compile_scoped_fn
  ) where

import           Control.Monad.State.Lazy                    (MonadState (..), modify)
import           Data.Typeable                               (Proxy (..))
--
import qualified Data.Text.Lazy                              as T
--
import qualified Data.Map.Strict                             as M'
--
import           YulDSL.Core
--
import           YulDSL.CodeGens.Yul.Internal.CodeFormatters
import           YulDSL.CodeGens.Yul.Internal.CodeGen


do_compile_cat :: Indenter -> AnyYulCat -> [Val] -> CGState CGOutput
do_compile_cat ind (MkAnyYulCat cat) vals_a = go cat where
  -- code-gen utilities
  --
  ret_vars vars = return ("", vars)
  ret_expr expr = return ("", [ValExpr expr])
  mk_code' :: forall a b. YulO2 a b => Code -> Proxy a -> Proxy b -> Code -> Code
  mk_code' = mk_code ind vals_a
  wrap_let_vars = \case Nothing -> id; Just vars -> \body -> ind (vars <> " {") <> body <> ind "}"
  -- go functions
  go :: forall a b. YulO2 a b => YulCat a b -> CGState CGOutput
  go YulCoerce        = ret_vars vals_a -- return (coerce_vals ind (Proxy @a) (Proxy @b) vals_a, vals_a)
  go YulSplit         = ret_vars vals_a -- return (mk_code' "split"  (Proxy @a) (Proxy @b) "", vals_a)
  go YulId            = ret_vars vals_a
  go (YulComp cb ac)  = go_comp cb ac
  go (YulProd ab cd)  = go_prod ab cd
  go (YulSwap @m @n)  = ret_vars (swap_vals (Proxy @m) (Proxy @n) vals_a)
  go (YulFork ab ac)  = go_intro ab ac
  go (YulExl @mn @m)  = go_extract (Proxy @mn) (Proxy @m) True
  go (YulExr @mn @n)  = go_extract (Proxy @mn) (Proxy @n) False
  go YulDis           = ret_vars (dis_vals (Proxy @a) vals_a)
  go YulDup           = go_dup (Proxy @a)
  go YulSGet          = ret_expr $ "sload(" <> vals_to_code vals_a <> ")"
  go YulSPut          = return (ind ("sstore(" <> vals_to_code vals_a <> ")"), [])
  go (YulEmbed a)     = ret_vars $ fmap (ValExpr . T.pack) (abi_type_list_vars a)
  -- go (YulCall c)        =
  go (YulJump i c)    = go_jump (Proxy @a) (Proxy @b) i c
  -- go (YulMap (MkFn n _)) = _
  -- go (YulFoldl (MkFn n _)) = _
  -- go (YulCall (MkFn n _)) = _
  go (YulITE @m)      = go_ite (Proxy @m)
  go YulNot           = ret_expr $ "not(" <> vals_to_code vals_a <> ")"
  go YulAnd           = ret_expr $ "and(" <> vals_to_code vals_a <> ")"
  go YulOr            = ret_expr $ "or(" <> vals_to_code vals_a <> ")"
  go YulNumAdd        = ret_expr $ "add(" <> vals_to_code vals_a <> ")"
  go YulNumNeg        = ret_expr $ "sub(0, " <> vals_to_code vals_a <> ")"
  go (YulNumCmp @m s) = go_num_cmp s (Proxy @m)
  go _                = error $ "do_compile_cat unimpl:" <> abi_type_uniq_name @a <> " ~> " <> abi_type_uniq_name @b -- FIXME remove
  go_comp :: forall a b c. YulO3 a b c => YulCat c b -> YulCat a c -> CGState CGOutput
  go_comp cb ac = do
    (code_ac, vals_c) <- do_compile_cat ind (MkAnyYulCat ac) vals_a
    (code_cb, vals_b) <- do_compile_cat ind (MkAnyYulCat cb) vals_c
    out_vars <- declare_vars
    return ( mk_code' "comp" (Proxy @(c,b)) (Proxy @(a,c)) $
             wrap_let_vars out_vars (code_ac <> code_cb)
           , vals_b )
  go_intro :: forall a b c. YulO3 a b c => YulCat a b -> YulCat a c -> CGState CGOutput
  go_intro ab ac = do
    (code_ab, vars_b) <- do_compile_cat ind (MkAnyYulCat ab) vals_a
    (code_ac, vars_c) <- do_compile_cat ind (MkAnyYulCat ac) vals_a
    out_vars <- declare_vars
    return ( mk_code' "intro" (Proxy @(a,b)) (Proxy @(a,c)) $
             wrap_let_vars out_vars (code_ab <> code_ac)
           , vars_b <> vars_c)
  go_extract :: forall m n. YulO2 m n => Proxy m -> Proxy n -> Bool -> CGState CGOutput
  go_extract _ _ extractLeft = let cn = abi_type_count_vars @n
                               in return
    ( mk_code' "extract" (Proxy @m) (Proxy @n) ""
    , if extractLeft then take cn vals_a else drop cn vals_a)
  go_prod :: forall a b c d. YulO4 a b c d => YulCat a b -> YulCat c d -> CGState CGOutput
  go_prod ab cd = do
    (code_ab, vars_b1) <- do_compile_cat ind (MkAnyYulCat ab) (fst_vals (Proxy @a) (Proxy @c) vals_a)
    (code_cd, vars_b2) <- do_compile_cat ind (MkAnyYulCat cd) (snd_vals (Proxy @a) (Proxy @c) vals_a)
    out_vars <- declare_vars
    return ( mk_code' "prod" (Proxy @(a,b)) (Proxy @(c,d)) $
             wrap_let_vars out_vars (code_ab <> code_cd)
           , vars_b1 <> vars_b2 )
  go_dup :: forall a. YulO1 a => Proxy a -> CGState CGOutput
  go_dup _ = do
    vars_a1 <- mk_let_vars (Proxy @a)
    vars_a2 <- mk_let_vars (Proxy @a)
    out_vars <- declare_vars
    return ( mk_code' "dup" (Proxy @a) (Proxy @(a,a)) $
             wrap_let_vars out_vars ( assign_vars ind vars_a1 vals_a <>
                                      ind (vars_a2 !! 0 <> " := " <> vars_a1 !! 0) -- TODO better looking code?
                                      -- assign_vars ind vars_a2 vals_a
                                    )
           , fmap LetVar (vars_a1 <> vars_a2) )
  go_jump :: forall a b. YulO2 a b => Proxy a -> Proxy b -> String -> YulCat a b -> CGState CGOutput
  go_jump _ _ cid cat' = do
    modify (\d@(MkCGStateData { dependant_cats = deps }) -> d {
               dependant_cats = M'.insert cid (MkAnyYulCat cat') deps
               })
    -- vals_b <- fmap LetVar <$> mk_let_vars (Proxy @b)
    -- forget_vars -- we do in-place declaration immediately
    return ( ""
           , [ValExpr $  T.pack cid <> "(" <> vals_to_code vals_a <> ")"])
  -- code block for if-then-else statement
  go_ite :: forall a. YulO1 a => Proxy a -> CGState CGOutput
  go_ite _ = let ca = abi_type_count_vars @a in assert (length vals_a == 1 + 2 * ca)
    (do vars_b <- mk_let_vars (Proxy @a)
        let vals_b = fmap LetVar vars_b
        return ( mk_code' "ite" (Proxy @(BOOL, (a, a))) (Proxy @a) $
                 ind ("switch " <> val_to_code (vals_a !! 0)) <>
                 cbracket1 ind "case 0"
                 (vals_to_code vals_b <> " := " <> (vals_to_code . take ca . drop 1) vals_a) <>
                 cbracket1 ind "default"
                 (vals_to_code vals_b <> " := " <> (vals_to_code . drop (1 + ca)) vals_a)
               , vals_b ))
  -- code block for compare numbers
  go_num_cmp :: forall a. YulO1 a => (BOOL, BOOL, BOOL) -> Proxy a -> CGState CGOutput
  go_num_cmp (BOOL True , BOOL False, BOOL False) _ = go_num_cmp' "lt(" ")" (Proxy @a)
  go_num_cmp (BOOL True , BOOL True , BOOL False) _ = go_num_cmp' "iszero(gt(" "))" (Proxy @a)
  go_num_cmp (BOOL False, BOOL True , BOOL False) _ = go_num_cmp' "eq(" ")" (Proxy @a)
  go_num_cmp (BOOL False, BOOL True , BOOL True ) _ = go_num_cmp' "iszero(lt(" "))" (Proxy @a)
  go_num_cmp (BOOL False, BOOL False, BOOL True ) _ = go_num_cmp' "gt(" ")" (Proxy @a)
  go_num_cmp _ _                                    = error "go_num_cmp: invalid boolean-switches combo"
  go_num_cmp' :: forall a. YulO1 a => Code -> Code -> Proxy a -> CGState CGOutput
  go_num_cmp' op1 op2 _ = assert (length vals_a == 2) $
    return ("", [ValExpr $ op1 <> vals_to_code vals_a <> op2 ])

compile_cat :: forall a b. YulO2 a b => Indenter -> YulCat a b -> ([Var], [Var]) -> CGState Code
compile_cat ind acat (vars_a, vars_r) =
  cbracket_m ind ""
  (\ind' -> do
      (code, vals_b) <- do_compile_cat ind' (MkAnyYulCat acat) (fmap LetVar vars_a)
      pure $ code <> assign_vars ind' vars_r vals_b
  )

compile_one_fn :: forall a b. YulO2 a b => Indenter -> Fn a b -> CGState Code
compile_one_fn ind fn = do
  reset_var_gen
  vars_a <- mk_let_vars (Proxy @a)
  vars_b <- mk_let_vars (Proxy @b)
  forget_vars -- these variables will not be declared separately
  code <- compile_cat ind (fnCat fn) (vars_a, vars_b)
  return $
    ind ("function " <> T.pack (fnId fn) <>
         "(" <> T.intercalate ", " vars_a <> ")" <>
          (case vars_b of [] -> ""; _ -> " -> " <> T.intercalate ", " vars_b)
        ) <> code

compile_one_any_fn :: Indenter -> AnyFn -> CGState Code
compile_one_any_fn ind (MkAnyFn fn)= compile_one_fn ind fn

-- | Compile dependencies with a function id filter @fidFilter@.
compile_deps :: Indenter -> (String -> Bool) -> CGState [Code]
compile_deps ind fidFilter = do
  deps <- fmap (\(i, c) -> case c of (MkAnyYulCat cat) -> MkAnyFn (MkFn i cat))
          . filter (\(i, _) -> fidFilter i)
          . M'.toList
          . dependant_cats
          <$> get
  mapM (compile_one_any_fn ind) deps

compile_fn :: forall a b. YulO2 a b => Indenter -> Fn a b -> CGState Code
compile_fn ind fn = do
  main_code <- compile_one_fn ind fn
  deps_codes <- compile_deps ind (/= fnId fn)
  return $
    ind main_code <> "\n" <>
    T.intercalate (ind "") deps_codes

compile_scoped_fn :: Indenter -> ScopedFn -> CGState Code
compile_scoped_fn ind sfn = case removeScope sfn of MkAnyFn fn -> compile_one_fn ind fn
