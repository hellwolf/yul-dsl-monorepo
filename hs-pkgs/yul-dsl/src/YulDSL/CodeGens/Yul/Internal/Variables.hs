{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings   #-}
module YulDSL.CodeGens.Yul.Internal.Variables where
-- base
import Data.Char                                   (chr)
import Data.Function                               ((&))
-- text
import Data.Text.Lazy                              qualified as T
--
import YulDSL.Core
--
import YulDSL.CodeGens.Yul.Internal.CodeFormatters


-- A variable represented by its name.
type Var = T.Text

-- A value represented by a let-bound variable or an expression.
data Val = LetVar Var
         | ValExpr Code
         deriving (Eq, Show)

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
new_auto_var g@(MkAutoVarGen i0) = (cur_var g, MkAutoVarGen (i0 + 1))

-- | Generate a list of variables for the 'ABITypeable' a.
--
-- Examples:
-- >>> gen_vars 3
-- ["v_a","v_b","v_c"]
gen_vars :: Int -> [Var]
gen_vars n = snd $ foldr
             (\ _ (gen, vars) -> new_auto_var gen & \ (var, gen') -> (gen', vars <> [var]))
             (MkAutoVarGen 0, [])
             (drop 1 [0..n])

vars_to_code :: [Var] -> Code
vars_to_code = T.intercalate ", "

val_to_code :: Val -> Code
val_to_code x = case x of LetVar c -> c; ValExpr e -> e

vals_to_code :: [Val] -> Code
vals_to_code = T.intercalate ", " . map val_to_code

abi_type_count_vars :: forall a. ABITypeable a => Int
abi_type_count_vars = length (abiTypeInfo @a)

swap_vals :: forall a b. (HasCallStack, YulO2 a b) => [Val] -> [Val]
swap_vals vars = gen_assert_msg ("swap_vals" ++ show (vars, ca, cb))
                 (ca + cb == length vars)
  (let (va, vb) = splitAt ca vars in vb <> va)
  where ca = abi_type_count_vars @a
        cb = abi_type_count_vars @b

dis_vals :: forall a. (HasCallStack, YulO1 a) => [Val] -> [Val]
dis_vals vars = gen_assert_msg ("dis_vals" ++ show (vars, ca))
                (ca == length vars) []
  where ca = abi_type_count_vars @a

-- | Extract value expressions of the first type @a@.
fst_vals :: forall a b. (HasCallStack, YulO2 a b) => [Val] -> [Val]
fst_vals vars = gen_assert_msg ("fst_vals" ++ show (vars, ca, cb))
                (ca + cb == length vars) (take ca vars)
  where ca = abi_type_count_vars @a
        cb = abi_type_count_vars @b

-- | Extract value expressions of the second type @b@.
snd_vals :: forall a b. (HasCallStack, YulO2 a b) => [Val] -> [Val]
snd_vals vars = gen_assert_msg ("snd_vals" ++ show (vars, ca, cb))
                (ca + cb == length vars) (drop ca vars)
  where ca = abi_type_count_vars @a
        cb = abi_type_count_vars @b

-- | Assigning variables.
mk_aliases :: Indenter -> [Var] -> [Var] -> Code
mk_aliases ind varsTo varsFrom = gen_assert_msg ("mk_aliases" ++ show (varsTo, varsFrom))
                                 (length varsTo == length varsFrom) $
  T.intercalate "" (fmap (\(a,b) -> ind (a <> " := " <> b)) (zip varsTo varsFrom))

-- | Assigning expression @vals@ to variables @vars@.
assign_vars :: Indenter -> [Var] -> [Val] -> Code
assign_vars ind vars vals = gen_assert_msg ("assign_vars" ++ show (vars, vals))
                            (length vars == length vals) $
  T.intercalate "" (fmap (\(a,b) -> ind (a <> " := " <> b)) (zip vars (fmap val_to_code vals)))
