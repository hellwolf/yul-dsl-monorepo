{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings   #-}
module YulDSL.CodeGens.Yul.Internal.Variables where

-- base
import           Data.Char                                   (chr)
import           Data.Function                               ((&))
import           GHC.Stack                                   (HasCallStack)
-- text
import qualified Data.Text.Lazy                              as T
--
import           YulDSL.Core
--
import           YulDSL.CodeGens.Yul.Internal.CodeFormatters


gen_assert_msg :: HasCallStack => String -> Bool -> a -> a
gen_assert_msg msg False _ = error msg
gen_assert_msg _     _ x   = x

-- A variable represented by its name.
type Var = T.Text

-- A value represented by a let-bound variable or an expression.
data Val = LetVar Var
         | ValExpr Code
         deriving Eq

-- Variable name generator.
--

-- Variable generator state.
newtype AutoVarGen = MkAutoVarGen Int

-- Current variable name the generator: a, b, c,..aa, ab, ac,..
cur_var  :: AutoVarGen -> Var
cur_var (MkAutoVarGen i0) = "v_" <> T.pack (go i0) where
  go i = if i < 26 then [chr (i + 97)] else let (j, i') = i `divMod` 26 in [chr (j + 96)] <> go i'

-- Generate a new auto variable.
new_auto_var :: AutoVarGen -> (Var, AutoVarGen)
new_auto_var g@(MkAutoVarGen i0) = (cur_var g, MkAutoVarGen (i0 + 1))

-- Generate a list of variables for the 'ABITypeable' a.
--
-- Examples:
-- >>> gen_vars 3
-- ["v_a","v_b"]
gen_vars :: Int -> [Var]
gen_vars n = snd $ foldr
             (\ _ (gen, vars) -> new_auto_var gen & \ (var, gen') -> (gen', vars <> [var]))
             (MkAutoVarGen 0, [])
             (drop 1 [0..n])

-- is_let_var :: Val -> Bool
-- is_let_var x = case x of LetVar _ -> True; _ -> False

vars_to_code :: [Var] -> Code
vars_to_code = T.intercalate ", "

val_to_code :: Val -> Code
val_to_code x = case x of LetVar c -> c; ValExpr e -> e

vals_to_code :: [Val] -> Code
vals_to_code = T.intercalate ", " . map val_to_code

abi_type_count_vars :: forall a. ABITypeable a => Int
abi_type_count_vars = length (abiTypeInfo @a)

swap_vals :: forall a b. YulO2 a b => [Val] -> [Val]
swap_vals vars = gen_assert_msg "swap_vals" (ca + cb == length vars)
  (let (va, vb) = splitAt ca vars in vb <> va)
  where ca = abi_type_count_vars @a
        cb = abi_type_count_vars @b

dis_vals :: forall a. YulO1 a => [Val] -> [Val]
dis_vals vars = gen_assert_msg "dis_vals" (ca == length vars) []
  where ca = abi_type_count_vars @a

-- Extract value expressions of the first type @a@.
fst_vals :: forall a b. YulO2 a b => [Val] -> [Val]
fst_vals vars = gen_assert_msg "fst_vals" (ca + cb == length vars) (take ca vars)
  where ca = abi_type_count_vars @a
        cb = abi_type_count_vars @b

-- Extract value expressions of the second type @b@.
snd_vals :: forall a b. YulO2 a b => [Val] -> [Val]
snd_vals vars = gen_assert_msg "snd_vals" (ca + cb == length vars) (drop ca vars)
  where ca = abi_type_count_vars @a
        cb = abi_type_count_vars @b

-- Assigning variables.
mk_aliases :: HasCallStack => Indenter -> [Var] -> [Var] -> Code
mk_aliases ind varsTo varsFrom = gen_assert_msg "mk_aliases" (length varsTo == length varsFrom) $
  T.intercalate "" (fmap (\(a,b) -> ind (a <> " := " <> b)) (zip varsTo varsFrom))

-- Assigning expression @vals@ to variables @vars@.
assign_vars :: HasCallStack => Indenter -> [Var] -> [Val] -> Code
assign_vars ind vars vals = gen_assert_msg "assign_vars" (length vars == length vals) $
  T.intercalate "" (fmap (\(a,b) -> ind (a <> " := " <> b)) (zip vars (fmap val_to_code vals)))

-- coerce_vals :: forall a b. YulO2 a b => Indenter -> Proxy a -> Proxy b -> [Val] -> Code
-- coerce_vals ind pa pb vars = case (typeRep pa, typeRep pb) of
--   (ta, tb) | ta == tb -> ""
--            | otherwise -> ""
