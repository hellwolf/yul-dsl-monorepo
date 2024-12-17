{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings   #-}
module YulDSL.CodeGens.Yul.Internal.FunctionGen
  ( compile_cat
  , compile_fn
  , compile_scoped_fn
  ) where

-- base
import           Data.Typeable                               (Proxy (..))
import           GHC.Stack                                   (HasCallStack)
-- text
import qualified Data.Text.Lazy                              as T
--
import           YulDSL.Core
--
import           YulDSL.CodeGens.Yul.Internal.CodeFormatters
import           YulDSL.CodeGens.Yul.Internal.CodeGen
import           YulDSL.CodeGens.Yul.Internal.Variables


do_compile_cat :: HasCallStack
               => Indenter -> AnyYulCat -> [Val] -> CGState CGOutput
do_compile_cat ind (MkAnyYulCat @eff cat) vals_a = go cat where
  -- code-gen utilities
  --
  ret_vars vars = pure ("", vars)
  ret_expr expr = pure ("", [ValExpr expr])
  mk_code :: forall a b. YulO2 a b => T.Text -> Code -> Code
  mk_code title code = ind (
    "//dbg: +" <> title <> "(" <>
    vals_to_code vals_a <>
    " : "  <> T.pack (abiTypeCompactName @a) <>
    ") -> " <> T.pack (abiTypeCompactName @b)
    ) <>
    code <>
    ind ("//dbg: -" <> title)
  wrap_let_vars = \case Nothing -> id; Just vars -> \body -> ind (vars <> " {") <> body <> ind "}"
  -- go functions
  go :: forall a b. YulO2 a b => YulCat eff a b -> CGState CGOutput
  go (YulExtendType)    = ret_vars vals_a
  go (YulReduceType)    = ret_vars vals_a
  go (YulCoerceType)    = ret_vars vals_a
  go (YulSplit)         = ret_vars vals_a
  --
  go (YulId)            = ret_vars vals_a
  go (YulComp cb ac)    = go_comp cb ac
  go (YulProd ab cd)    = go_prod ab cd
  go (YulSwap @_ @m @n) = ret_vars (swap_vals (Proxy @m) (Proxy @n) vals_a)
  go (YulFork ab ac)    = go_fork ab ac
  go (YulExl @_ @mn @m) = go_extract (Proxy @mn) (Proxy @m) True  {- extractLeft -}
  go (YulExr @_ @mn @n) = go_extract (Proxy @mn) (Proxy @n) False {- extractLeft -}
  go (YulDis)           = ret_vars (dis_vals (Proxy @a) vals_a)
  go (YulDup)           = go_dup (Proxy @a)
  --
  go (YulEmb a)         = ret_vars $ fmap (ValExpr . T.pack) [show a]
  go (YulITE @_ @m)     = go_ite (Proxy @m)
  go (YulJmp tgt)       = go_jmp @a @b tgt
  --
  go (YulSGet)          = ret_expr $ "sload(" <> vals_to_code vals_a <> ")"
  go (YulSPut)          = return (ind ("sstore(" <> vals_to_code vals_a <> ")"), [])
  go_comp :: forall a b c. YulO3 a b c => YulCat eff c b -> YulCat eff a c -> CGState CGOutput
  go_comp cb ac = do
    (code_ac, vals_c) <- do_compile_cat ind (MkAnyYulCat ac) vals_a
    (code_cb, vals_b) <- do_compile_cat ind (MkAnyYulCat cb) vals_c
    out_vars <- cg_declare_vars
    return ( mk_code @(c, b) @(a, c) "comp" $
             wrap_let_vars out_vars (code_ac <> code_cb)
           , vals_b )
  go_fork :: forall a b c. YulO3 a b c => YulCat eff a b -> YulCat eff a c -> CGState CGOutput
  go_fork ab ac = do
    (code_ab, vars_b) <- do_compile_cat ind (MkAnyYulCat ab) vals_a
    (code_ac, vars_c) <- do_compile_cat ind (MkAnyYulCat ac) vals_a
    out_vars <- cg_declare_vars
    return ( mk_code @(a, b) @(a, c) "fork" $
             wrap_let_vars out_vars (code_ab <> code_ac)
           , vars_b <> vars_c)
  go_extract :: forall m n. YulO2 m n => Proxy m -> Proxy n -> Bool -> CGState CGOutput
  go_extract _ _ extractLeft = let leftVars = abi_type_count_vars @m
    in return ( mk_code @m @n ("extract" <> if extractLeft then "L" else "R") ""
              , if extractLeft then take leftVars vals_a else drop leftVars vals_a)
  go_prod :: forall a b c d. YulO4 a b c d => YulCat eff a b -> YulCat eff c d -> CGState CGOutput
  go_prod ab cd = do
    (code_ab, vars_b1) <- do_compile_cat ind (MkAnyYulCat ab) (fst_vals (Proxy @a) (Proxy @c) vals_a)
    (code_cd, vars_b2) <- do_compile_cat ind (MkAnyYulCat cd) (snd_vals (Proxy @a) (Proxy @c) vals_a)
    out_vars <- cg_declare_vars
    return ( mk_code @((a, b), (c, d)) @() "prod" $
             wrap_let_vars out_vars (code_ab <> code_cd)
           , vars_b1 <> vars_b2 )
  go_dup :: forall a. YulO1 a => Proxy a -> CGState CGOutput
  go_dup _ = do
    vars_a1 <- cg_mk_let_vars @a
    vars_a2 <- cg_mk_let_vars @a
    out_vars <- cg_declare_vars
    return ( mk_code @a @() "dup" $
             wrap_let_vars out_vars ( assign_vars ind vars_a1 vals_a <>
                                      mk_aliases ind vars_a2 vars_a1
                                    )
           , fmap LetVar (vars_a1 <> vars_a2) )
  go_jmp :: forall a b. YulO2 a b => YulJmpTarget eff a b -> CGState CGOutput
  go_jmp (UserDefinedYulCat (depId, depCat)) = do
    -- modify (\d@(MkCGStateData { dependent_cats = deps }) -> d {
    --            dependent_cats = Map.insert cid (MkAnyYulCat cat') deps
    --            })
    cg_insert_dependent_cat depId (MkAnyYulCat depCat)
    return ( ""
           , [ValExpr $  T.pack depId <> "(" <> vals_to_code vals_a <> ")"])
  go_jmp (BuiltInYulJmpTarget (builtinName, _)) = do
    -- modify (\d@(MkCGStateData { builtin_used = builtins }) -> d {
    --            builtin_used = Set.insert cid builtins
    --            })
    cg_use_builtin builtinName
    return ( ""
           , [ValExpr $  T.pack builtinName <> "(" <> vals_to_code vals_a <> ")"])
  -- code block for if-then-else statement
  go_ite :: forall a. YulO1 a => Proxy a -> CGState CGOutput
  go_ite _ = let nouts = abi_type_count_vars @a
             in gen_assert_msg ("vals_a len: "<> show (length vals_a)) (length vals_a == 1 + 2 * nouts)
    (do vars_b <- map LetVar <$> cg_mk_let_vars @a
        return ( mk_code @(BOOL, (a, a)) @a "ite" $
                 ind ("switch " <> val_to_code (vals_a !! 0)) <>
                 cbracket1 ind "case 1"
                 (vals_to_code vars_b <> " := " <> (vals_to_code . take nouts . drop 1) vals_a) <>
                 cbracket1 ind "default"
                 (vals_to_code vars_b <> " := " <> (vals_to_code . drop (1 + nouts)) vals_a)
               , vars_b ))

compile_cat :: forall a b eff. (HasCallStack, YulO2 a b)
            => Indenter -> YulCat eff a b -> ([Var], [Var]) -> CGState Code
compile_cat ind acat (vars_a, vars_r) = do
  (code, vals_b) <- do_compile_cat ind (MkAnyYulCat acat) (fmap LetVar vars_a)
  pure $
    code <>
    assign_vars ind vars_r vals_b

compile_fn :: forall a b eff. (HasCallStack, YulO2 a b)
           => Indenter -> FnCat eff a b -> CGState Code
compile_fn ind f = do
  cg_reset_var_gen
  vars_a <- cg_mk_let_vars @a
  vars_b <- cg_mk_let_vars @b
  cg_forget_vars -- these variables will not be declared separately
  cbracket_m ind ("function " <> T.pack (fnId f) <>
                   "(" <> T.intercalate ", " vars_a <> ")" <>
                   (case vars_b of [] -> ""; _ -> " -> " <> T.intercalate ", " vars_b)
                 )
    ( \ind' -> do
        code <- compile_cat ind' (fnCat f) (vars_a, vars_b)
        pure $
          code <>
          ind' "leave"
    )

compile_scoped_fn :: HasCallStack
                  => Indenter -> ScopedFn -> CGState Code
compile_scoped_fn ind f = case unScopedFn f of MkAnyFnCat f' -> compile_fn ind f'
