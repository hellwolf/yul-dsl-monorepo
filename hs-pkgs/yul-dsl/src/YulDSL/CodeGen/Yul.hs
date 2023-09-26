{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module YulDSL.CodeGen.Yul
  ( compileFn
  , compileObject
  ) where

-- import           Control.Exception        (assert)
import           Control.Monad.State.Lazy (State, evalState, get, modify, put)
import           Data.Char                (chr)
import qualified Data.Map.Strict          as M'
import           Data.Maybe               (fromJust, isJust)
import qualified Data.Text.Lazy           as T
import           Data.Typeable            (Proxy (..))

import           YulDSL.Core


assert _ a = a

------------------------------------------------------------------------------------------------------------------------
-- Code and Formatting
------------------------------------------------------------------------------------------------------------------------

-- Code is text.
type Code = T.Text

-- Indentation formatter.
type Indenter = T.Text -> T.Text

-- Add one level of indentation.
add_indent :: T.Text -> T.Text
add_indent s = " " <> s

-- Add one level of indentation.
indent :: Indenter -> Indenter
indent ind = \s -> add_indent (ind s)

-- Initial indentation.
init_ind :: Indenter
init_ind = \s -> s <> "\n"

------------------------------------------------------------------------------------------------------------------------
-- Variables
------------------------------------------------------------------------------------------------------------------------

-- | A variable represented by its name.
type Var = T.Text

-- | A value represented by a let-bound variable or an expression.
data Val = LetVar Var | ValExpr Code deriving Eq

-- Variable name generator.
--

-- | Variable generator state.
newtype AutoVarGen = MkAutoVarGen Int

-- | Current variable name the generator: a, b, c,..aa, ab, ac,..
cur_var  :: AutoVarGen -> Var
cur_var (MkAutoVarGen i0) = "v_" <> T.pack (go i0) where
  go i = if i < 26 then [chr (i + 97)] else let (j, i') = i `divMod` 26 in [chr (j + 96)] <> go i'

-- | Generate a new auto variable.
new_auto_var :: AutoVarGen -> (Var, AutoVarGen)
new_auto_var g@(MkAutoVarGen i0) = (cur_var g, MkAutoVarGen (i0 + 1)) where

-- Variable and value
--

-- is_let_var :: Val -> Bool
-- is_let_var x = case x of LetVar _ -> True; _ -> False

val_to_code :: Val -> Code
val_to_code x = case x of LetVar c -> c; ValExpr e -> e

vals_to_code :: [Val] -> Code
vals_to_code = T.intercalate "," . map val_to_code

swap_vals :: forall a b. YulO2 a b => Proxy a -> Proxy b -> [Val] -> [Val]
swap_vals _ _ vars = assert (ca + cb == length vars)
  (let (va, vb) = splitAt ca vars in vb <> va)
  where ca = abi_type_count_vars @a
        cb = abi_type_count_vars @b

dis_vals :: forall a. YulO1 a => Proxy a -> [Val] -> [Val]
dis_vals _ vars = assert (ca == length vars) []
  where ca = abi_type_count_vars @a

fst_vals :: forall a b. YulO2 a b => Proxy a -> Proxy b -> [Val] -> [Val]
fst_vals _ _ vars = assert (ca + cb == length vars) (take ca vars)
  where ca = abi_type_count_vars @a
        cb = abi_type_count_vars @b

snd_vals :: forall a b. YulO2 a b => Proxy a -> Proxy b -> [Val] -> [Val]
snd_vals _ _ vars = assert (ca + cb == length vars) (drop ca vars)
  where ca = abi_type_count_vars @a
        cb = abi_type_count_vars @b

------------------------------------------------------------------------------------------------------------------------
-- CodeGen (CG) Machinery
------------------------------------------------------------------------------------------------------------------------

-- | CodeGen state data.
data CGStateData = MkCGStateData { var_gen         :: AutoVarGen
                                 , undeclared_vars :: [Var]
                                 , dependant_cats  :: M'.Map String AnyYulCat -- cat_id -> cat
                                 }

type CGState = State CGStateData

type CGOutput = (Code, [Val])

init_cg :: CGStateData
init_cg = MkCGStateData { var_gen = MkAutoVarGen 0
                        , undeclared_vars = []
                        , dependant_cats = M'.empty
                        }

reset_var_gen :: CGState ()
reset_var_gen = do
  modify (\s -> (s { var_gen = MkAutoVarGen 0
                  , undeclared_vars = []
                  }))

next_var :: CGState Var
next_var = do
  s <- get
  let (v, g) = new_auto_var (var_gen s)
  put (s { var_gen = g
         , undeclared_vars = v : undeclared_vars s
         })
  return v

mk_let_vars :: forall a proxy. YulO1 a => proxy a -> CGState [Var]
mk_let_vars _ = return . reverse =<< go (abi_type_count_vars @a) []
  where go n vars = next_var >>= \var ->
          if n > 1 then go (n - 1) (var:vars) else return (var:vars)

forget_vars :: CGState ()
forget_vars = modify $ \s -> s { undeclared_vars = [] }

assign_vars :: Indenter -> [Var] -> [Val] -> Code
assign_vars ind vars vals = assert (length vars == length vals) $
    T.intercalate "" (fmap (\(a,b) -> ind (a <> " := " <> b)) (zip vars (fmap val_to_code vals)))

declare_vars :: Indenter -> CGState Code
declare_vars ind = do
  s <- get
  let vars = undeclared_vars s
      code = if length vars == 0 then "" else ind ("let " <> T.intercalate ", " vars)
  put (s { undeclared_vars = [] })
  return code

mk_code :: forall a b. YulO2 a b => Indenter -> [Val] -> T.Text -> Proxy a -> Proxy b -> Code -> Code
mk_code ind vals title _ _ code = ind (
  "//dbg: " <> title <> " " <>
  vals_to_code vals <>
  " : "  <> T.pack (abi_type_name @a) <>
  " -> " <> T.pack (abi_type_name @b)
  ) <> code

-- coerce_vals :: forall a b. YulO2 a b => Indenter -> Proxy a -> Proxy b -> [Val] -> Code
-- coerce_vals ind pa pb vars = case (typeRep pa, typeRep pb) of
--   (ta, tb) | ta == tb -> ""
--            | otherwise -> ""

gen_code :: Indenter -> AnyYulCat -> [Val] -> CGState CGOutput
gen_code ind (MkAnyYulCat cat) vals_a = go cat where
  -- code-gen utilities
  --
  ind' = indent ind
  ret_vars vars = return ("", vars)
  ret_expr expr = return ("", [ValExpr expr])
  mk_code' :: forall a b. YulO2 a b => T.Text -> Proxy a -> Proxy b -> Code -> Code
  mk_code' = mk_code ind vals_a
  wrap_let_vars :: Code -> (Code -> Code)
  wrap_let_vars vars = if vars == "" then id else \x -> vars <> ind "{" <> x <> ind "}"
  -- go functions
  --
  go :: forall a b. YulO2 a b => YulCat a b -> CGState CGOutput
  go YulCoerce        = ret_vars vals_a -- return (coerce_vals ind (Proxy @a) (Proxy @b) vals_a, vals_a)
  go YulSplit         = return (mk_code' "split"  (Proxy @a) (Proxy @b) "", vals_a)
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
  go (YulEmbed a)     = ret_vars $ fmap (ValExpr . T.pack) (abi_type_show_vars a)
  -- go (YulCall c)        =
  go (YulJump i c)    = go_jump (Proxy @a) (Proxy @b) i c
  -- go (YulMap (MkFn n _)) = _
  -- go (YulFoldl (MkFn n _)) = _
  -- go (YulCall (MkFn n _)) = _
  go YulITE           = go_ite (Proxy @a)
  go YulNot           = ret_expr $ "not(" <> vals_to_code vals_a <> ")"
  go YulAnd           = ret_expr $ "and(" <> vals_to_code vals_a <> ")"
  go YulOr            = ret_expr $ "or(" <> vals_to_code vals_a <> ")"
  go YulNumAdd        = ret_expr $ "add(" <> vals_to_code vals_a <> ")"
  go YulNumNeg        = ret_expr $ "sub(0, " <> vals_to_code vals_a <> ")"
  go (YulNumCmp @m s) = go_num_cmp s (Proxy @m)
  go _                = error $ "gen_code unimpl:" <> abi_type_name @a <> " ~> " <> abi_type_name @b -- FIXME remove
  go_comp :: forall a b c. YulO3 a b c => YulCat c b -> YulCat a c -> CGState CGOutput
  go_comp cb ac = do
    (code_ac, vals_c) <- gen_code ind (MkAnyYulCat ac) vals_a
    (code_cb, vals_b) <- gen_code ind (MkAnyYulCat cb) vals_c
    code_vars <- declare_vars ind
    return ( mk_code' "comp" (Proxy @(c,b)) (Proxy @(a,c)) $ wrap_let_vars code_vars (code_ac <> code_cb)
           , vals_b )
  go_intro :: forall a b c. YulO3 a b c => YulCat a b -> YulCat a c -> CGState CGOutput
  go_intro ab ac = do
    (code_ab, vars_b) <- gen_code ind (MkAnyYulCat ab) vals_a
    (code_ac, vars_c) <- gen_code ind (MkAnyYulCat ac) vals_a
    code_vars <- declare_vars ind
    return ( mk_code' "intro" (Proxy @(a,b)) (Proxy @(a,c)) $ wrap_let_vars code_vars (code_ab <> code_ac)
           , vars_b <> vars_c)
  go_extract :: forall m n. YulO2 m n => Proxy m -> Proxy n -> Bool -> CGState CGOutput
  go_extract _ _ extractLeft = let cn = abi_type_count_vars @n
                               in return
    ( mk_code' "extract" (Proxy @m) (Proxy @n) ""
    , if extractLeft then take cn vals_a else drop cn vals_a)
  go_prod :: forall a b c d. YulO4 a b c d => YulCat a b -> YulCat c d -> CGState CGOutput
  go_prod ab cd = do
    (code_ab, vars_b1) <- gen_code ind (MkAnyYulCat ab) (fst_vals (Proxy @a) (Proxy @c) vals_a)
    (code_cd, vars_b2) <- gen_code ind (MkAnyYulCat cd) (snd_vals (Proxy @a) (Proxy @c) vals_a)
    code_vars <- declare_vars ind
    return ( mk_code' "prod" (Proxy @(a,b)) (Proxy @(c,d)) $ wrap_let_vars code_vars (code_ab <> code_cd)
           , vars_b1 <> vars_b2 )
  go_dup :: forall a. YulO1 a => Proxy a -> CGState CGOutput
  go_dup _ = do
    vars_a1 <- mk_let_vars (Proxy @a)
    vars_a2 <- mk_let_vars (Proxy @a)
    code_vars <- declare_vars ind
    return ( mk_code' "dup" (Proxy @a) (Proxy @(a,a)) $
             wrap_let_vars code_vars ( assign_vars ind vars_a1 vals_a <>
                                       assign_vars ind vars_a2 vals_a )
           , fmap LetVar (vars_a1 <> vars_a2) )
  go_jump :: forall a b. YulO2 a b => Proxy a -> Proxy b -> String -> YulCat a b -> CGState CGOutput
  go_jump _ _ cid cat = do
    modify (\d@(MkCGStateData { dependant_cats = deps }) -> d {
               dependant_cats = M'.insert cid (MkAnyYulCat cat) deps
               })
    vals_b <- return . fmap LetVar =<< mk_let_vars (Proxy @b)
    forget_vars -- we do in-place declaration immediately
    return ( mk_code' "jump" (Proxy @a) (Proxy @b) $
             ind ("let " <> vals_to_code vals_b <> " := " <>
                  T.pack cid <> "(" <> vals_to_code vals_a <> ")")
           , vals_b )
  go_ite :: forall a. YulO1 a
           => Proxy a -> CGState CGOutput
  go_ite _ = let ca = abi_type_count_vars @a in assert (length vals_a == 1 + 2 * ca)
    (do vars_b <- mk_let_vars (Proxy @a)
        let vals_b = fmap LetVar vars_b
        return ( mk_code' "ite" (Proxy @(BOOL, (a, a))) (Proxy @a) $
                 ind ("switch " <> val_to_code (vals_a !! 0)) <>
                 ind "case 0 {" <>
                 ind' (vals_to_code vals_b <> " := " <> (vals_to_code . take ca . drop 1) vals_a) <>
                 ind "}" <>
                 ind "default {" <>
                 ind' (vals_to_code vals_b <> " := " <> (vals_to_code . drop (1 + ca)) vals_a) <>
                 ind "}"
               , vals_b ))
  go_num_cmp :: forall a. YulO1 a => (BOOL, BOOL, BOOL) -> Proxy a -> CGState CGOutput
  go_num_cmp (BOOL True , BOOL False, BOOL False) _ = go_num_cmp' "lt(" ")" (Proxy @a)
  go_num_cmp (BOOL True , BOOL True , BOOL False) _ = go_num_cmp' "iszero(gt(" "))" (Proxy @a)
  go_num_cmp (BOOL False, BOOL True , BOOL False) _ = go_num_cmp' "eq(" ")" (Proxy @a)
  go_num_cmp (BOOL False, BOOL True , BOOL True ) _ = go_num_cmp' "iszero(lt(" "))" (Proxy @a)
  go_num_cmp (BOOL False, BOOL False, BOOL True ) _ = go_num_cmp' "gt(" ")" (Proxy @a)
  go_num_cmp _ _                                    = error "go_num_cmp: invalid boolean-switches combo"
  go_num_cmp' :: forall a. YulO1 a => Code -> Code -> Proxy a -> CGState CGOutput
  go_num_cmp' op1 op2 _ = assert (length vals_a == 2) (return ("", [ValExpr $ op1 <> vals_to_code vals_a <> op2 ]))

------------------------------------------------------------------------------------------------------------------------
-- Yul Object Builders
------------------------------------------------------------------------------------------------------------------------

compile_cat :: forall a b. YulO2 a b => Indenter -> YulCat a b -> ([Var], [Var]) -> CGState Code
compile_cat ind acat (vars_a, vars_r) = do
  let ind' = indent ind
  (code, vals_b) <- gen_code ind' (MkAnyYulCat acat) (fmap LetVar vars_a)
  return $
    "{\n" <>
    code <>
    assign_vars ind' vars_r vals_b <>
    ind "}"

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
        ) <>
    ind code
compile_one_any_fn :: Indenter -> AnyFn -> CGState Code
compile_one_any_fn ind (MkAnyFn fn)= compile_one_fn ind fn

compile_deps :: Indenter -> (String -> Bool) -> CGState [Code]
compile_deps ind fidFilter = do
  deps <- fmap (\(i, c) -> case c of (MkAnyYulCat cat) -> MkAnyFn (MkFn i cat))
          <$> filter (\(i, _) -> fidFilter i)
          <$> M'.toList <$> dependant_cats <$> get
  mapM (compile_one_any_fn ind) deps

compile_fn :: forall a b. YulO2 a b => Indenter -> Fn a b -> CGState Code
compile_fn ind fn = do
  main_code <- compile_one_fn ind fn
  deps_codes <- compile_deps ind (/= (fnId fn))
  return $
    ind main_code <> "\n" <>
    T.intercalate (ind "") deps_codes

compile_scoped_fn :: Indenter -> ScopedFn -> CGState Code
compile_scoped_fn ind sfn = case removeScope sfn of MkAnyFn fn -> compile_one_fn ind fn

create_dispatcher :: Indenter -> [ScopedFn] -> CGState Code
create_dispatcher ind fns = do
  code_cases <- (mapM case_fn . fmap fromJust . filter isJust . fmap dispatchable) fns
                >>= return . T.intercalate ""
  return $
    "{ // Dispatcher\n" <>
    ind' "switch selector()" <>
    code_cases <>
    ind' "default { revert(0, 0) }" <>
    ind' "function selector() -> s {" <>
    ind' "  s := div(calldataload(0), 0x100000000000000000000000000000000000000000000000000000000)" <>
    ind' "}" <>
    ind "}"
  where ind' = indent ind
        ind'' = indent ind'
        dispatchable (ExternalFn _ sel fn) = Just (sel, MkAnyYulCat (fnCat fn))
        dispatchable (LibraryFn _)         = Nothing
        case_fn (sel@(SEL (sig, Just (fname, _))), (MkAnyYulCat (_ :: YulCat a b))) = do
          vars_a <- mk_let_vars (Proxy @a)
          vars_b <- mk_let_vars (Proxy @b)
          code_vars <- declare_vars ind''
          return $
            ind' ("case " <> T.pack (show sel) <> "{") <>
            code_vars <>
            ind'' "// TODO, abi decoding of input" <>
            ind'' (T.intercalate "," vars_b <> " := " <>
                   T.pack fname <> "(" <> T.intercalate "," vars_a <> ")") <>
            ind' "}"

compile_object :: Indenter -> YulObject -> CGState Code
compile_object ind (MkYulObject { yulObjectName = oname
                                , yulObjectCtor = ctor
                                , yulObjectSFns = sfns
                                , yulSubObjects = subos
                                }) = do
  let ind' = indent ind
      ind'' = indent ind'
      ind''' = indent ind''
  code_ctor <- compile_cat ind'' ctor ([], [])
  code_dispatcher <- create_dispatcher ind''' sfns
  code_fns <- mapM (compile_scoped_fn ind''') sfns -- exported functions
  code_subos <- mapM (compile_object ind') subos -- sub objects
  return $
    ind ("object \"" <> T.pack oname <> "\" {") <>
    ( -- object init
      ind' "code {" <>
      (
        ind'' "datacopy(0, dataoffset(\"runtime\"), datasize(\"runtime\"))" <>
        ind'' "" <>
        ind'' "// constructor" <>
        ind'' code_ctor <>
        ind'' "return(0, datasize(\"runtime\"))"
      ) <>
      ind' "}" <>
      -- runtime object
      ind' "object \"runtime\" {" <>
      (
        ind'' "code {" <>
        ind''' code_dispatcher <>
        T.intercalate "\n" code_fns <>
        ind'' "}"
      ) <>
      ind' "}"
    ) <> "\n" <>
    T.intercalate "\n" code_subos <>
    ind "}"

-- Module Exports:
--

-- | Compiling a yul function.
compileFn :: forall a b p. YulO2 a b => Fn a b -> Code
compileFn fn = evalState (compile_fn init_ind fn) init_cg

-- | Compiling the yul object.
compileObject :: YulObject -> Code
compileObject obj = evalState (compile_object init_ind obj) init_cg
