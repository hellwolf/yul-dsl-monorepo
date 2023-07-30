{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module YulDSL.CodeGen.Yul
  ( compileFn
  , compileObject
  ) where

import           Control.Monad.State.Lazy (State, evalState, get, put)
import           Data.Char                (chr)
import qualified Data.Text                as T
import           Data.Typeable            (Proxy (..))

import           YulDSL.Core


------------------------------------------------------------------------------------------------------------------------
-- Code and Formatting
------------------------------------------------------------------------------------------------------------------------

type Code = T.Text

-- | Indentation formater.
type Indenter = T.Text -> T.Text

indent :: Indenter -> Indenter
indent ind = \s -> " " <> ind s

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

data Vars = OneVars [Var] | ProdVars Vars Vars

------------------------------------------------------------------------------------------------------------------------
-- Compilation Machinery
------------------------------------------------------------------------------------------------------------------------

-- | Cat compilation state.
data CatStateData = MkCatStateData { varGen :: AutoVarGen
                                   }

type CatState = State CatStateData

init_catst :: CatStateData
init_catst = MkCatStateData { varGen = MkAutoVarGen 0
                            }
next_var :: CatState Var
next_var = do
  s <- get
  let (v, g) = new_auto_var (varGen s)
  put (s { varGen = g })
  return v

mk_vars :: forall a proxy. ABIType a => proxy a -> CatState Vars
mk_vars _ = go (abi_type_count_vars @a) (OneVars [])
  where go n (OneVars vars) = next_var >>= \var ->
          if n > 1 then go (n - 1) (OneVars (var:vars)) else return (OneVars (var:vars))
        go _ _ = undefined -- impossible paths

let_vars :: Vars -> T.Text
let_vars (OneVars [])       = ""
let_vars (OneVars [v])      = v
let_vars (OneVars (v:vs))   = v <> ", " <> let_vars (OneVars vs)
let_vars (ProdVars vs1 vs2) = let_vars vs1 <> ", " <> let_vars vs2 -- ??

dis_vars :: Vars -> Vars
dis_vars _ = OneVars []

swap_vars :: Vars -> Vars
swap_vars (ProdVars vs1 vs2) = ProdVars vs2 vs1
swap_vars _                  = error "Not a product"

fst_vars :: Vars -> Vars
fst_vars (ProdVars vs1 _) = vs1
fst_vars _                = error "Not a product"

snd_vars :: Vars -> Vars
snd_vars (ProdVars _ vs2) = vs2
snd_vars _                = error "Not a product"

dup_vars :: Vars -> Vars
dup_vars vs = ProdVars vs vs

run_stcat :: Indenter -> AnyYulCat -> (Vars, Vars) -> CatState Code
run_stcat ind (MkAnyYulCat cat) (a_vars, b_vars) = go cat where
  go :: forall a b. YulO2 a b =>
        YulCat a b -> CatState Code
  go YulCoerce             = return $ comment_it "TODO coerce" (Proxy :: Proxy a) (Proxy :: Proxy b)
  go YulId                 = return $ comment_it "TODO id" (Proxy :: Proxy a) (Proxy :: Proxy b)
  go YulSwap               = return $ comment_it "TODO swap" (Proxy :: Proxy a) (Proxy :: Proxy b)
  go YulDis                = return $ comment_it "TODO dis" (Proxy :: Proxy a) (Proxy :: Proxy b)
  go YulDup                = return $ comment_it "TODO dup" (Proxy :: Proxy a) (Proxy :: Proxy b)
  go (YulComp YulId ab)    = go ab
  go (YulComp ab YulId)    = go ab
  go (YulComp YulSwap ab') = go_comp1 ab' id swap_vars
  go (YulComp a'b YulSwap) = go_comp1 a'b swap_vars id
  go (YulComp YulDis ab')  = go_comp1 ab' id dis_vars
  go (YulComp a'b YulDis)  = go_comp1 a'b dis_vars id
  go (YulComp YulDup ab')  = go_comp1 ab' id dup_vars
  go (YulComp a'b YulDup)  = go_comp1 a'b dup_vars id
  go (YulComp cb ac)       = go_comp2 cb ac
  go (YulProd ab cd)       = go_prod ab cd
  go YulNot                = return.ind $ let_vars b_vars <> " := not(" <> let_vars a_vars <> ")"
  go YulAnd                = return.ind $ let_vars b_vars <> " := and(" <> let_vars a_vars <> ")"
  go YulOr                 = return.ind $ let_vars b_vars <> " := or(" <> let_vars a_vars <> ")"
  go YulNumAdd             = return.ind $ let_vars b_vars <> " := add(" <> let_vars a_vars <> ")"
  go YulNumNeg             = return.ind $ let_vars b_vars <> " := sub(0, " <> let_vars a_vars <> ")"
  go (YulNumCmp _)         = return $ comment_it "TODO numcmp" (Proxy :: Proxy a) (Proxy :: Proxy b)
  go YulSGet               = return $ comment_it "TODO sget" (Proxy :: Proxy a) (Proxy :: Proxy b)
  go YulSPut               = return $ comment_it "TODO sput" (Proxy :: Proxy a) (Proxy :: Proxy b)
  go (YulEmbed a)          = return "" -- return (T.pack (show a))
  go (YulApFun (MkFn n _)) = return $ comment_it "TODO apfun" (Proxy :: Proxy a) (Proxy :: Proxy b)
  go YulITE                = return $ comment_it "TODO ite" (Proxy :: Proxy a) (Proxy :: Proxy b)
  go _                     = error $ "run_stcat " <> abi_type_name @a <> " ~> " <> abi_type_name @b
  -- go YulITE = return $ "ite"
  -- go (YulMap (MkFn n _)) = _
  -- go (YulFoldl (MkFn n _)) = _
  -- go (YulCall (MkFn n _)) = _
  comment_it :: forall a b. YulO2 a b
           => T.Text -> Proxy a -> Proxy b -> Code
  comment_it t _ _ = ind $ "/* " <> t <> " " <> T.pack(abi_type_name @a <> " ~> " <> abi_type_name @b) <> " */"
  go_comp1 :: forall a b. YulO2 a b
           => YulCat a b -> (Vars -> Vars) -> (Vars -> Vars) -> CatState Code
  go_comp1 ab fa fb = do
    let ind' = indent ind
    ab_code <- run_stcat ind' (MkAnyYulCat ab) (fa a_vars, fb b_vars)
    return $
      comment_it "comp1" (Proxy :: Proxy a) (Proxy :: Proxy b) <>
      ind "{" <> ab_code <> ind "}"
  go_comp2 :: forall a b c. YulO3 a b c
           => YulCat c b -> YulCat a c -> CatState Code
  go_comp2 cb ac = do
    let ind' = indent ind
    c_vars <- mk_vars (Proxy :: Proxy c)
    ac_code <- run_stcat ind' (MkAnyYulCat ac) (a_vars, c_vars)
    cb_code <- run_stcat ind' (MkAnyYulCat cb) (c_vars, b_vars)
    return $
      comment_it "comp2" (Proxy :: Proxy (c,b)) (Proxy :: Proxy (a,c)) <>
      ind ("let " <> let_vars c_vars ) <>
      ind "{" <> ac_code <> ind "}" <>
      ind "{" <> cb_code <> ind "}"
  go_prod :: forall a b c d. YulO4 a b c d
          => YulCat a b -> YulCat c d -> CatState Code
  go_prod ab cd = do
    let ind' = indent ind
    ab_code <- run_stcat ind' (MkAnyYulCat ab) (fst_vars a_vars, fst_vars b_vars)
    cd_code <- run_stcat ind' (MkAnyYulCat cd) (snd_vars a_vars, snd_vars b_vars)
    return $
      comment_it "prod" (Proxy :: Proxy (a,b)) (Proxy :: Proxy (c,d)) <>
      ind "{" <> ab_code <> ind "}" <>
      ind "{" <> cd_code <> ind "}"

compile_cat :: forall a b. (ABIType a, ABIType b) => Indenter -> YulCat a b -> (Vars, Vars) -> CatState Code
compile_cat ind acat (a_vars, b_vars) = do
  let ind' = indent ind
  code <- run_stcat ind' (MkAnyYulCat acat) (a_vars, b_vars)
  return $ ind "{" <> code <> ind "}"

compile_fn :: forall a b. Indenter -> Fn a b -> CatState Code
compile_fn ind (MkFn n cat) = do
  a_vars <- mk_vars (Proxy :: Proxy a)
  b_vars <- mk_vars (Proxy :: Proxy b)
  code <- compile_cat ind cat (a_vars, b_vars)
  return $
    ind ("function " <> T.pack n <>
         "(" <> let_vars a_vars <> ")" <>
         (case b_vars of (OneVars []) -> ""; _ -> " -> " <> let_vars b_vars)
        ) <> code

compile_code :: Indenter -> YulCode -> CatState Code
compile_code ind (MkYulCode { yulFunctions = fns, yulInitCode = ic }) = do
  let ind' = indent ind
  init_code <- compile_cat (indent ind) ic (OneVars [], OneVars [])
  fns_code <- mapM (\(MkAnyFn fn) -> compile_fn ind' fn) fns
  return $ ind "code {" <>
    init_code <> "\n" <>
    T.intercalate "\n" fns_code <>
    ind "}"

compile_object :: Indenter -> YulObject -> CatState Code
compile_object ind (MkYulObject { yulObjectName = n, yulObjectCode = c, yulSubObjects = os }) = do
  let ind' = indent ind
  obj_code <- compile_code ind' c
  subobj_codes <- mapM (compile_object ind') os
  return $ ind ("object \"" <> T.pack n <> "\"{") <>
    obj_code <> "\n" <>
    T.intercalate "\n" subobj_codes <> "\n" <>
    ind "}"

------------------------------------------------------------------------------------------------------------------------
-- Module Exports:
------------------------------------------------------------------------------------------------------------------------

-- | Compiling the yul function.
compileFn :: Fn a b -> Code
compileFn fn = evalState (compile_fn init_ind fn) init_catst

-- | Compiling the yul object.
compileObject :: YulObject -> Code
compileObject obj = evalState (compile_object init_ind obj) init_catst
