{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module YulDSL.CodeGen.Yul
  ( compileCat
  , compileCode
  , compileFn
  , compileObject
  ) where

import           Control.Monad.State.Lazy (State, get, put, runState)
import           Data.Char                (chr)
import qualified Data.Text                as T
import           YulDSL.Core.YulCat
import           YulDSL.Core.YulObject


type Var = T.Text
type Code = T.Text

newtype AutoVarGen = MkAutoVarGen Int

cur_var  :: AutoVarGen -> Var
cur_var (MkAutoVarGen i0) = T.pack (go i0) where
  go i = if i < 26 then [chr (i + 97)] else let (j, i') = i `divMod` 26 in [chr (j + 96)] <> go i'

-- | Generate a new auto variable.
new_auto_var :: AutoVarGen -> (Var, AutoVarGen)
new_auto_var g@(MkAutoVarGen i0) = (cur_var g, MkAutoVarGen (i0 + 1)) where

-- | Indentation formater.
type Indenter = T.Text -> T.Text

indent :: Indenter -> Indenter
indent ind = \s -> " " <> ind s

init_ind :: Indenter
init_ind = \s -> s <> "\n"

-- | Compiling the category.
compileCat :: YulCat a b -> Code
compileCat = compile_cat init_ind . MkAnyYulCat

-- | Cat compilation state.
data CatState = MkCataStte { varGen :: AutoVarGen
                           }

init_cat_state :: CatState
init_cat_state = MkCataStte { varGen = MkAutoVarGen 0
                            }

compile_cat :: Indenter -> AnyYulCat -> Code
compile_cat ind0 acat = ind0 "{" <> fst runGo <> ind0 "}" where
  runGo = runState (go' (indent ind0) acat) init_cat_state
  next_var = do
    s <- get
    let (v, g) = new_auto_var (varGen s)
    put (s { varGen = g })
    return v
  go' :: Indenter -> AnyYulCat -> State CatState Code
  go' ind (MkAnyYulCat cat) = go cat where
    go :: YulCat a b -> State CatState Code
    go YulCoerce          = return $ ind "/* TODO: coerce */"
    go YulId              = return ""
    go (YulComp YulId ab) = go ab
    go (YulComp bc YulId) = go bc
    go (YulComp bc ab) = do
      v1 <- next_var
      c1 <- go' (indent ind) (MkAnyYulCat ab)
      v2 <- next_var
      c2 <- go' (indent ind) (MkAnyYulCat bc)
      return $
        ind ("let " <> v1) <>
        ind "{" <> c1 <> ind "}" <>
        ind ("let " <> v2) <>
        ind "{" <> c2 <> ind "}"
    go (YulProd c d) = return $ ind "/* TODO: prod */"
    go YulSwap       = return $ ind "/* TODO: swap */"
    go YulDis        = return $ ind "/* TODO: dis */"
    go YulDup        = return $ ind "/* TODO: dup */"
    -- go (YulEmbed a)  = return $ T.pack(show a)
    go (YulApFun (MkFn n _)) = return $ ind (T.pack(n) <> "()")
    -- go YulITE = return $ "ite"
    go _             = return ""

compileFn :: Fn a b -> Code
compileFn = compile_fn init_ind

compile_fn :: Indenter -> Fn a b -> Code
compile_fn ind (MkFn n cat) = ind ("function " <> T.pack n <> "()") <>
                              compile_cat ind (MkAnyYulCat cat)

compileCode ::  YulCode -> Code
compileCode = compile_code init_ind

compile_code :: Indenter -> YulCode -> Code
compile_code ind (MkYulCode { yulFunctions = fns, yulInitCode = ic }) =
  ind "code {" <>
  compile_cat (indent ind) (MkAnyYulCat ic) <> "\n" <>
  T.intercalate "\n" (fmap (\(MkAnyFn fn) -> compile_fn (indent ind) fn) fns) <>
  ind "}"

compileObject :: YulObject -> Code
compileObject = go init_ind
  where go ind (MkYulObject { yulObjectName = n, yulObjectCode = c, yulSubObjects = os }) =
          ind ("object \"" <> (T.pack n) <> "\"{") <>
          -- include code
          compile_code (indent ind) c <>
          -- include sub objects
          T.intercalate "\n" (fmap (go (indent ind)) os) <> "\n" <>
          ind "}"
