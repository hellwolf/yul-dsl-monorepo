{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings   #-}
module YulDSL.CodeGens.Yul.Internal.FunctionGen
  ( compile_cat
  , compile_fn
  , compile_scoped_fn
  ) where

-- base
-- text
import Data.Text.Lazy                              qualified as T
--
import YulDSL.Core
--
import YulDSL.CodeGens.Yul.Internal.CodeFormatters
import YulDSL.CodeGens.Yul.Internal.CodeGen
import YulDSL.CodeGens.Yul.Internal.Variables


-- A value represented by a let-bound variable or an expression.
data Val = LetVar Var
         | ValExpr Code
  deriving Show

val_to_code :: Val -> Code
val_to_code x = case x of LetVar c -> unVar c; ValExpr e -> e

vals_to_code :: [Val] -> Code
vals_to_code = T.intercalate ", " . map val_to_code

-- | Aliasing variables.
mk_aliases :: HasCallStack => Indenter -> [Var] -> [Var] -> Code
mk_aliases ind varsTo varsFrom = gen_assert_msg ("mk_aliases" ++ show (varsTo, varsFrom))
                                 (length varsTo == length varsFrom) $
  T.intercalate "" (fmap
                    (\(MkVar a, MkVar b) -> ind (a <> " := " <> b))
                    (zip varsTo varsFrom))

-- | Assigning expression @vals@ to variables @vars@.
assign_vars :: HasCallStack => Indenter -> [Var] -> [Val] -> Code
assign_vars ind vars vals = gen_assert_msg ("assign_vars" ++ show (vars, vals))
                            (length vars == length vals) $
  T.intercalate "" (fmap
                    (\(MkVar a, b) -> ind (a <> " := " <> b))
                    (zip vars (fmap val_to_code vals)))

type CGOutput = (Code, [Val])

ret_vals :: [Val] -> CGState CGOutput
ret_vals vals = pure ("", vals)

do_compile_cat :: HasCallStack
               => Indenter -> AnyYulCat -> [Val] -> CGState CGOutput
do_compile_cat ind (MkAnyYulCat @eff cat) vals_a = go cat where
  -- code gen helpers

  wrap_let_vars :: Maybe Code -> (Code -> Code)
  wrap_let_vars = \case
    Just vars -> \body -> ind (vars <> " {") <> body <> ind "}"
    Nothing -> id

  mk_code :: T.Text -> Code -> Code
  mk_code title code =
    ind ("//dbg: +" <> title) <>
    code <>
    ind ("//dbg: -" <> title)

  -- go functions
  go :: forall a b. YulO2 a b => YulCat eff a b -> CGState CGOutput
  -- - type conversions
  go (YulExtendType)     = ret_vals vals_a
  go (YulReduceType)     = ret_vals vals_a
  go (YulCoerceType)     = ret_vals vals_a
  go (YulSplit)          = ret_vals vals_a
  -- - categorical
  go (YulId)             = ret_vals vals_a
  go (YulComp cb ac)     = go_comp cb ac
  go (YulProd ab cd)     = go_prod ab cd
  go (YulSwap @_ @m @n)  = go_swap @m @n
  go (YulFork ab ac)     = go_fork ab ac
  go (YulExl @_ @m @n)   = go_extract @m @n True  {- extractLeft -}
  go (YulExr @_ @m @n)   = go_extract @m @n False {- extractLeft -}
  go (YulDis)            = go_dis @a
  go (YulDup)            = go_dup @a
  -- - control flows
  go (YulEmb @_ @m @n x) = go_emb @m @n x
  go (YulITE @_ @m)      = go_ite @m
  go (YulJmp tgt)        = go_jmp @a @b tgt
  -- - storage effects
  go (YulSGet)           = ret_vals [ ValExpr ("sload(" <> vals_to_code vals_a <> ")") ]
  go (YulSPut)           = pure (ind ("sstore(" <> vals_to_code vals_a <> ")"), [])

  --
  go_emb :: forall a b. (HasCallStack, YulO2 a b)
         => b -> CGState CGOutput
  go_emb b = -- FIXME: proper implementation of embeddable
    case length (abiTypeInfo @b) of
      0 -> pure("", []) -- unit type
      1 -> ret_vals [ ValExpr (T.pack (show b)) ]
      _ -> error ("Unembedable: " ++ show b)
  --
  go_comp :: forall a b c. (HasCallStack, YulO3 a b c)
          => YulCat eff c b -> YulCat eff a c -> CGState CGOutput
  go_comp cb ac = do
    let title = T.pack $ "comp " ++
          "(" ++ abiTypeCompactName @a ++ ") -> " ++
          "(" ++ abiTypeCompactName @c ++ ") -> " ++
          "(" ++ abiTypeCompactName @b ++ ")"
    (code_ac, vals_c) <- do_compile_cat ind (MkAnyYulCat ac) vals_a
    (code_cb, vals_b) <- do_compile_cat ind (MkAnyYulCat cb) vals_c
    out_vars <- cg_declare_vars
    pure ( mk_code title $
           wrap_let_vars out_vars (code_ac <> code_cb)
         , vals_b )
  --
  go_prod :: forall a b c d. (HasCallStack, YulO4 a b c d)
          => YulCat eff a b -> YulCat eff c d -> CGState CGOutput
  go_prod ab cd = do
    let ca = length (abiTypeInfo @a)
        cc = length (abiTypeInfo @c)
        (fst_vals, snd_vals) = gen_assert_msg ("fst_vals" ++ show (vals_a, ca, cc))
                               (ca + cc == length vals_a)
                               (take ca vals_a, drop ca vals_a)
    (code_ab, vars_b1) <- do_compile_cat ind (MkAnyYulCat ab) fst_vals
    (code_cd, vars_b2) <- do_compile_cat ind (MkAnyYulCat cd) snd_vals
    let title = T.pack $ "prod " ++
          ("(" ++ abiTypeCompactName @a ++ ", " ++ abiTypeCompactName @c ++ ") -> ") ++
          ("(" ++ abiTypeCompactName @b ++ ", " ++ abiTypeCompactName @d ++ ")")
    out_vars <- cg_declare_vars
    pure ( mk_code title $
           wrap_let_vars out_vars (code_ab <> code_cd)
         , vars_b1 <> vars_b2 )
  --
  go_swap :: forall a b. (HasCallStack, YulO2 a b)
          => CGState CGOutput
  go_swap = gen_assert_msg ("swap " ++ show (vals_a, ca, cb))
            (ca + cb == length vals_a)
            (let (va, vb) = splitAt ca vals_a
             in ret_vals (vb <> va))
    where ca = length (abiTypeInfo @a)
          cb = length (abiTypeInfo @b)
  --
  go_fork :: forall a b c. (HasCallStack, YulO3 a b c)
          => YulCat eff a b -> YulCat eff a c -> CGState CGOutput
  go_fork ab ac = do
    let title = T.pack $ "fork " ++
          "(" ++ abiTypeCompactName @a ++ ") -> " ++
          "(" ++ abiTypeCompactName @b ++ ", " ++ abiTypeCompactName @c ++ ")"
    (code_ab, vars_b) <- do_compile_cat ind (MkAnyYulCat ab) vals_a
    (code_ac, vars_c) <- do_compile_cat ind (MkAnyYulCat ac) vals_a
    out_vars <- cg_declare_vars
    return ( mk_code title $
             wrap_let_vars out_vars (code_ab <> code_ac)
           , vars_b <> vars_c)
  --
  go_extract :: forall a b. (HasCallStack, YulO2 a b)
             => Bool -> CGState CGOutput
  go_extract extractLeft =
    let title = T.pack $ ("extract" <> if extractLeft then "L " else "R ") ++
          ("(" ++ abiTypeCompactName @a ++ ", " ++ abiTypeCompactName @b ++ ")")
        leftVars = length (abiTypeInfo @a)
    in return ( mk_code title ""
              , if extractLeft then take leftVars vals_a else drop leftVars vals_a)
  --
  go_dis :: forall a. (HasCallStack, YulO1 a)
         => CGState CGOutput
  go_dis = gen_assert_msg ("dis_vals " ++ show (vals_a, ca))
           (ca == length vals_a)
           (ret_vals [])
    where ca = length (abiTypeInfo @a)
  --
  go_dup :: forall a. (HasCallStack, YulO1 a)
         => CGState CGOutput
  go_dup = do
    let title = T.pack $ "dup (" ++ abiTypeCompactName @a ++ ")"
    vars_a1 <- cg_create_vars @a
    vars_a2 <- cg_create_vars @a
    out_vars <- cg_declare_vars
    return ( mk_code title $
             wrap_let_vars out_vars ( assign_vars ind vars_a1 vals_a <>
                                      mk_aliases ind vars_a2 vars_a1
                                    )
           , fmap LetVar (vars_a1 <> vars_a2) )
  --
  go_jmp :: forall a b. (HasCallStack, YulO2 a b)
         => YulJmpTarget eff a b -> CGState CGOutput
  go_jmp tgt = do
    fname <- case tgt of
      (UserDefinedYulCat (depId, depCat))    -> cg_insert_dependent_cat depId (MkAnyYulCat depCat) >> pure depId
      (BuiltInYulJmpTarget (builtinName, _)) -> cg_use_builtin builtinName >> pure builtinName
    let title = T.pack $ "jmp " ++ fname ++
          "(" ++ abiTypeCompactName @a ++ ") -> (" ++ abiTypeCompactName @b ++ ")"
        callExpr = T.pack fname <> "(" <> vals_to_code vals_a <> ")"
    if length (abiTypeInfo @b) == 1
      then do return ("" , [ValExpr callExpr])
      else do vars_b <- cg_create_vars @b
              pure ( mk_code title $
                     ind (vars_to_code vars_b <> " := " <> callExpr)
                     -- ind callExpr
                   , fmap LetVar vars_b )
  --
  go_ite :: forall a. (HasCallStack, YulO1 a)
         => CGState CGOutput
  go_ite = let nouts = length (abiTypeInfo @a)
           in gen_assert_msg ("vals_a len: "<> show (vals_a, nouts))
              (length vals_a == 1 + 2 * nouts)
    (do let title = T.pack $ "ite (" ++ abiTypeCompactName @a ++ ")"
        vals_b <- map LetVar <$> cg_create_vars @a
        return ( mk_code title $
                 ind ("switch " <> val_to_code (vals_a !! 0)) <>
                 cbracket1 ind "case 1"
                 (vals_to_code vals_b <> " := " <> (vals_to_code . take nouts . drop 1) vals_a) <>
                 cbracket1 ind "default"
                 (vals_to_code vals_b <> " := " <> (vals_to_code . drop (1 + nouts)) vals_a)
               , vals_b ))

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
  vars_a <- cg_create_vars @a
  vars_b <- cg_create_vars @b
  cg_forget_vars -- these variables will not be declared separately

  code <- cbracket_m ind
    ( "function " <> T.pack (fnId f) <>
      "(" <> vars_to_code vars_a <> ")" <>
      (if null vars_b then "" else " -> " <> vars_to_code vars_b)
    )
    ( \ind' -> do
        body <- compile_cat ind' (fnCat f) (vars_a, vars_b)
        pure $ body <> ind' "leave"
    )

  cg_reset_for_fn
  pure code

compile_scoped_fn :: HasCallStack
                  => Indenter -> ScopedFn -> CGState Code
compile_scoped_fn ind f = case unScopedFn f of MkAnyFnCat f' -> compile_fn ind f'
