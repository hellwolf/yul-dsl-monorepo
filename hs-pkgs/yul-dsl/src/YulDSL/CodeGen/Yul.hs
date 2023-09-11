{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module YulDSL.CodeGen.Yul
  ( compileCat
  , compileFn
  , compileObject
  ) where

import           Control.Exception        (assert)
import           Control.Monad.State.Lazy (State, evalState, get, put)
import           Data.Char                (chr)
import           Data.Maybe               (fromJust, isJust)
import qualified Data.Text                as T
import           Data.Typeable            (Proxy (..), typeRep)

import           YulDSL.Core


------------------------------------------------------------------------------------------------------------------------
-- Code and Formatting
------------------------------------------------------------------------------------------------------------------------

type Code = T.Text

-- | Indentation formater.
type Indenter = T.Text -> T.Text

add_indent :: T.Text -> T.Text
add_indent s = " " <> s

indent :: Indenter -> Indenter
indent ind = \s -> add_indent (ind s)

init_ind :: Indenter
init_ind = \s -> s <> "\n"

------------------------------------------------------------------------------------------------------------------------
-- Variables
------------------------------------------------------------------------------------------------------------------------

type Var = T.Text

newtype AutoVarGen = MkAutoVarGen Int

cur_var  :: AutoVarGen -> Var
cur_var (MkAutoVarGen i0) = "v_" <> T.pack (go i0) where
  go i = if i < 26 then [chr (i + 97)] else let (j, i') = i `divMod` 26 in [chr (j + 96)] <> go i'

-- | Generate a new auto variable.
new_auto_var :: AutoVarGen -> (Var, AutoVarGen)
new_auto_var g@(MkAutoVarGen i0) = (cur_var g, MkAutoVarGen (i0 + 1)) where

data Val = LetVar Var
         | ValExpr Code
         deriving Eq

------------------------------------------------------------------------------------------------------------------------
-- Compilation Machinery
------------------------------------------------------------------------------------------------------------------------

-- | Cat compilation state.
data CatStateData = MkCatStateData { var_gen         :: AutoVarGen
                                   , undeclared_vars :: [Var]
                                   }

type CatState = State CatStateData

init_catst :: CatStateData
init_catst = MkCatStateData { var_gen = MkAutoVarGen 0
                            , undeclared_vars = []
                            }
next_var :: CatState Var
next_var = do
  s <- get
  let (v, g) = new_auto_var (var_gen s)
  put (s { var_gen = g
         , undeclared_vars = v : undeclared_vars s
         })
  return v

forget_vars :: CatState ()
forget_vars = get >>= \s -> put (s { undeclared_vars = [] })

declare_vars :: Indenter -> CatState Code
declare_vars ind = do
  s <- get
  let vars = undeclared_vars s
      code = if length vars == 0 then "" else ind ("let " <> T.intercalate ", " vars)
  put (s { undeclared_vars = [] })
  return code

mk_let_vars :: forall a proxy. YulO1 a => proxy a -> CatState [Var]
mk_let_vars _ = return . reverse =<< go (abi_type_count_vars @a) []
  where go n vars = next_var >>= \var ->
          if n > 1 then go (n - 1) (var:vars) else return (var:vars)

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

coerce_vals :: forall a b. YulO2 a b => Indenter -> Proxy a -> Proxy b -> [Val] -> Code
coerce_vals ind pa pb vars = case (typeRep pa, typeRep pb) of
  (ta, tb) | ta == tb -> ""
           | otherwise -> "" -- error "coerce_vals otherwise"

assign_vars :: Indenter -> [Var] -> [Val] -> Code
assign_vars ind vars vals = assert (length vars == length vals) $
  T.intercalate "" (fmap (\(a,b) -> ind (a <> " := " <> b)) (zip vars (fmap val_to_code vals)))

run_stcat :: Indenter -> AnyYulCat -> [Val] -> CatState (Code, [Val])
run_stcat ind (MkAnyYulCat cat) vals_a = go cat where
  go :: forall a b. YulO2 a b =>
        YulCat a b -> CatState (Code, [Val])
  go YulCoerce          = return (coerce_vals ind (Proxy @a) (Proxy @b) vals_a, vals_a)
  go YulId              = ret_nocode vals_a
  go (YulSwap @m @n)    = ret_nocode (swap_vals (Proxy @m) (Proxy @n) vals_a)
  go YulDis             = ret_nocode (dis_vals (Proxy @a) vals_a)
  go YulDup             = go_dup (Proxy @a)
  go (YulComp cb ac)    = go_comp cb ac
  go (YulProd ab cd)    = go_prod ab cd
  go YulSGet            = ret_1expr $ "sload(" <> vals_to_code vals_a <> ")"
  go YulSPut            = return (ind ("sstore(" <> vals_to_code vals_a <> ")"), [])
  go (YulEmbed a)       = return ("", fmap (ValExpr . T.pack) (abi_type_show_vars a))
  -- go (YulCall c)        = go_call c
  go (YulJump c)        = go_jump (Proxy @a) (Proxy @b) c
  go YulITE             = go_ite (Proxy @b)
  go YulNot             = ret_1expr $ "not(" <> vals_to_code vals_a <> ")"
  go YulAnd             = ret_1expr $ "and(" <> vals_to_code vals_a <> ")"
  go YulOr              = ret_1expr $ "or(" <> vals_to_code vals_a <> ")"
  go YulNumAdd          = ret_1expr $ "add(" <> vals_to_code vals_a <> ")"
  go YulNumNeg          = ret_1expr $ "sub(0, " <> vals_to_code vals_a <> ")"
  go (YulNumCmp @m cmp) = go_num_cmp cmp (Proxy @m)
  go _                  = error $ "run_stcat " <> abi_type_name @a <> " ~> " <> abi_type_name @b
  -- go (YulMap (MkFn n _)) = _
  -- go (YulFoldl (MkFn n _)) = _
  -- go (YulCall (MkFn n _)) = _
  ind' = indent ind
  ret_nocode vars = return ("", vars)
  ret_1expr  expr = return ("", [ValExpr expr])
  log_debug :: forall a b. YulO2 a b
           => T.Text -> Proxy a -> Proxy b -> Code
  log_debug t _ _ = ind $
    "//dbg: " <> t <> " " <>
    vals_to_code vals_a <>
    " : "  <> T.pack (abi_type_name @a) <>
    " -> " <> T.pack (abi_type_name @b)
  wrap_let_vars vars = if vars == "" then id else \x -> vars <> ind "{" <> x <> ind "}"
  go_dup :: forall a. YulO1 a
         => Proxy a -> CatState (Code, [Val])
  go_dup _ = do
    vars_a1 <- mk_let_vars (Proxy @a)
    vars_a2 <- mk_let_vars (Proxy @a)
    code_vars <- declare_vars ind
    return ( log_debug "dup" (Proxy @a) (Proxy @(a,a)) <>
             wrap_let_vars code_vars ( assign_vars ind vars_a1 vals_a <>
                                       assign_vars ind vars_a2 vals_a )
           , fmap LetVar (vars_a1 <> vars_a2) )
  go_comp :: forall a b c. YulO3 a b c
          => YulCat c b -> YulCat a c -> CatState (Code, [Val])
  go_comp cb ac = do
    (code_ac, vals_c) <- run_stcat ind (MkAnyYulCat ac) vals_a
    (code_cb, vals_b) <- run_stcat ind (MkAnyYulCat cb) vals_c
    code_vars <- declare_vars ind
    return ( log_debug "comp" (Proxy @(c,b)) (Proxy @(a,c)) <>
             wrap_let_vars code_vars ( code_ac <> code_cb )
           , vals_b )
  go_prod :: forall a b c d. YulO4 a b c d
          => YulCat a b -> YulCat c d -> CatState (Code, [Val])
  go_prod ab cd = do
    (code_ab, vars_b1) <- run_stcat ind (MkAnyYulCat ab) (fst_vals (Proxy @a) (Proxy @c) vals_a)
    (code_cd, vars_b2) <- run_stcat ind (MkAnyYulCat cd) (snd_vals (Proxy @a) (Proxy @c) vals_a)
    code_vars <- declare_vars ind
    return ( log_debug "prod" (Proxy @(a,b)) (Proxy @(c,d)) <>
             wrap_let_vars code_vars ( code_ab <> code_cd )
           , vars_b1 <> vars_b2 )
  go_jump :: forall a b. YulO2 a b
           => Proxy a -> Proxy b -> String -> CatState (Code, [Val])
  go_jump _ _ n = do
    vals_b <- return . fmap LetVar =<< mk_let_vars (Proxy @b)
    forget_vars -- we do in-place declaration immediately
    return ( log_debug "apfun" (Proxy @a) (Proxy @b) <>
             ind ("let " <> vals_to_code vals_b <> " := " <>
                  T.pack n <> "(" <> vals_to_code vals_a <> ")")
           , vals_b )
  go_ite :: forall a. YulO1 a
           => Proxy a -> CatState (Code, [Val])
  go_ite _ = let ca = abi_type_count_vars @a in assert (length vals_a == 1 + 2 * ca)
    (do vars_b <- mk_let_vars (Proxy @a)
        let vals_b = fmap LetVar vars_b
        return ( log_debug "ite" (Proxy @(BOOL, (a, a))) (Proxy @a) <>
                 ind ("switch " <> val_to_code (vals_a !! 0)) <>
                 ind "case 0 {" <>
                 ind' (vals_to_code vals_b <> " := " <> (vals_to_code . take ca . drop 1) vals_a) <>
                 ind "}" <>
                 ind "default {" <>
                 ind' (vals_to_code vals_b <> " := " <> (vals_to_code . drop (1 + ca)) vals_a) <>
                 ind "}"
               , vals_b ))
  go_num_cmp :: forall a. YulO1 a
             => (BOOL, BOOL, BOOL) -> Proxy a -> CatState (Code, [Val])
  go_num_cmp (BOOL True, BOOL False, BOOL False) _ = go_num_cmp' "lt(" ")" (Proxy @a)
  go_num_cmp (BOOL True, BOOL True, BOOL False)  _ = go_num_cmp' "iszero(gt(" "))" (Proxy @a)
  go_num_cmp (BOOL False, BOOL True, BOOL False) _ = go_num_cmp' "eq(" ")" (Proxy @a)
  go_num_cmp (BOOL False, BOOL True, BOOL True)  _ = go_num_cmp' "iszero(lt(" "))" (Proxy @a)
  go_num_cmp (BOOL False, BOOL False, BOOL True) _ = go_num_cmp' "gt(" ")" (Proxy @a)
  go_num_cmp _ _                                   = error "go_num_cmp: invalid boolean-switches combo"
  go_num_cmp' :: forall a. YulO1 a
              => Code -> Code -> Proxy a -> CatState (Code, [Val])
  go_num_cmp' op1 op2 _ = assert (length vals_a == 2) (return ("", [ValExpr $ op1 <> vals_to_code vals_a <> op2 ]))

compile_cat :: forall a b. YulO2 a b => Indenter -> YulCat a b -> ([Var], [Var]) -> CatState Code
compile_cat ind acat (vars_a, vars_r) = do
  let ind' = indent ind
  (code, vals_b) <- run_stcat ind' (MkAnyYulCat acat) (fmap LetVar vars_a)
  return $
    "{\n" <>
    code <>
    coerce_vals ind' (Proxy @b) (Proxy @b) vals_b <>
    assign_vars ind' vars_r vals_b <>
    ind "}"

compile_fn :: forall a b. YulO2 a b => Indenter -> Fn a b -> CatState Code
compile_fn ind fn =
  let cat = removeScope fn
      fname = case fn of (ExternalFn _ _ _) -> digestYulCat cat
                         (LibraryFn n _)    -> n
  in do
    vars_a <- mk_let_vars (Proxy @a)
    vars_b <- mk_let_vars (Proxy @b)
    forget_vars -- these variables will not be declared separately
    code <- compile_cat ind cat (vars_a, vars_b)
    return $
      ind ("function " <> T.pack fname <>
           "(" <> T.intercalate ", " vars_a <> ")" <>
           (case vars_b of [] -> ""; _ -> " -> " <> T.intercalate ", " vars_b)
          ) <>
      ind code

compile_any_fn :: Indenter -> AnyFn -> CatState Code
compile_any_fn ind (MkAnyFn fn) = compile_fn ind fn

create_dispatcher :: Indenter -> [AnyFn] -> CatState Code
create_dispatcher ind fns = do
  code_cases <- (mapM case_fn . fmap fromJust . filter isJust . fmap dispatchable) fns
                >>= return . T.intercalate ""
  return $
    "{ // Dispatcher\n" <>
    ind' "switch selector()" <>
    code_cases <>
    ind' "default { revert(0, 0) }" <>
    ind' "function selector() -> s {" <>
    ind' " s := div(calldataload(0), 0x100000000000000000000000000000000000000000000000000000000)" <>
    ind' "}" <>
    ind "}"
  where ind' = indent ind
        ind'' = indent ind'
        dispatchable (MkAnyFn (ExternalFn _ sel c)) = Just (sel, MkAnyYulCat c)
        dispatchable (MkAnyFn (LibraryFn _ _))      = Nothing
        case_fn (s, (MkAnyYulCat (c :: YulCat a b))) = do
          vars_a <- mk_let_vars (Proxy @a)
          vars_b <- mk_let_vars (Proxy @b)
          code_vars <- declare_vars ind''
          return $
            ind' ("case " <> T.pack (show s) <> "{") <>
            code_vars <>
            ind'' "// TODO, abi decoding of input" <>
            ind'' (T.intercalate "," vars_b <> " := " <>
                   T.pack (digestYulCat c) <> "(" <> T.intercalate "," vars_a <> ")") <>
            ind' "}"

compile_object :: Indenter -> YulObject -> CatState Code
compile_object ind (MkYulObject { yulObjectName = oname
                                , yulObjectCtor = ctor
                                , yulObjectFunctions = fns
                                , yulSubObjects = subos
                                }) = do
  let ind' = indent ind
      ind'' = indent ind'
      ind''' = indent ind''
  code_ctor <- compile_cat ind'' ctor ([], [])
  code_dispatcher <- create_dispatcher ind''' fns
  code_fns <- mapM (compile_any_fn ind''') fns -- exported functions
  code_subos <- mapM (compile_object ind') subos -- sub objects
  return $
    ind ("object \"" <> T.pack oname <> "\" {") <>
    ( -- object init
      ind' "code {" <>
      (
        ind'' "sstore(0, caller()) // store the creator in slot zero" <>
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

------------------------------------------------------------------------------------------------------------------------
-- Module Exports:
------------------------------------------------------------------------------------------------------------------------

-- | Compiling the yul morphism.
compileCat :: forall a b p. YulO2 a b => YulCat a b -> Code
compileCat c = evalState (compile_cat init_ind c ([], [])) init_catst

-- | Compiling the yul exported function.
compileFn :: forall a b p. YulO2 a b => Fn a b -> Code
compileFn fn = evalState (compile_fn init_ind fn) init_catst

-- | Compiling the yul object.
compileObject :: YulObject -> Code
compileObject obj = evalState (compile_object init_ind obj) init_catst
