{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings   #-}

module YulDSL.CodeGens.Yul.Internal.CodeGen where

-- base
import           Control.Monad.State.Lazy                    (MonadState (..), State, evalState, modify)
import           Data.Char                                   (chr)
import           Data.Function                               ((&))
import           Data.Typeable                               (Proxy (..))
import           GHC.Stack                                   (HasCallStack)
-- text
import qualified Data.Text.Lazy                              as T
-- containers
import qualified Data.Map.Strict                             as M
import qualified Data.Set                                    as S
--
import           YulDSL.Core
--
import           YulDSL.CodeGens.Yul.Internal.CodeFormatters

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
new_auto_var g@(MkAutoVarGen i0) = (cur_var g, MkAutoVarGen (i0 + 1))

-- | Generate a list of variables for the 'ABITypeable' a.
--
-- Examples:
-- >>> gen_vars (Proxy @(INT256, BOOL))
-- ["v_a","v_b"]
gen_vars :: Int -> [Var]
gen_vars n = snd $ foldr
             (\ _ (gen, vars) -> new_auto_var gen & \ (var, gen') -> (gen', vars <> [var]))
             (MkAutoVarGen 0, [])
             (drop 1 [0..n])

-- is_let_var :: Val -> Bool
-- is_let_var x = case x of LetVar _ -> True; _ -> False

val_to_code :: Val -> Code
val_to_code x = case x of LetVar c -> c; ValExpr e -> e

vals_to_code :: [Val] -> Code
vals_to_code = T.intercalate ", " . map val_to_code

swap_vals :: forall a b. YulO2 a b => Proxy a -> Proxy b -> [Val] -> [Val]
swap_vals _ _ vars = gen_assert (ca + cb == length vars)
  (let (va, vb) = splitAt ca vars in vb <> va)
  where ca = abi_type_count_vars @a
        cb = abi_type_count_vars @b

dis_vals :: forall a. YulO1 a => Proxy a -> [Val] -> [Val]
dis_vals _ vars = gen_assert (ca == length vars) []
  where ca = abi_type_count_vars @a

-- | Extract value expressions of the first type @a@.
fst_vals :: forall a b. YulO2 a b => Proxy a -> Proxy b -> [Val] -> [Val]
fst_vals _ _ vars = gen_assert (ca + cb == length vars) (take ca vars)
  where ca = abi_type_count_vars @a
        cb = abi_type_count_vars @b

-- | Extract value expressions of the second type @b@.
snd_vals :: forall a b. YulO2 a b => Proxy a -> Proxy b -> [Val] -> [Val]
snd_vals _ _ vars = gen_assert (ca + cb == length vars) (drop ca vars)
  where ca = abi_type_count_vars @a
        cb = abi_type_count_vars @b

------------------------------------------------------------------------------------------------------------------------
-- CodeGen (CG) Machinery
------------------------------------------------------------------------------------------------------------------------

-- | CodeGen state data.
data CGStateData = MkCGStateData { var_gen         :: AutoVarGen
                                 , undeclared_vars :: [Var]
                                 , dependent_cats  :: M.Map String AnyYulCat -- cat_id -> cat
                                 , builtin_used    :: S.Set String
                                 }

type CGState = State CGStateData

-- TODO remove this
type CGOutput = (Code, [Val])

init_cg :: CGStateData
init_cg = MkCGStateData { var_gen = MkAutoVarGen 0
                        , undeclared_vars = []
                        , dependent_cats = M.empty
                        , builtin_used = S.empty
                        }

reset_var_gen :: CGState ()
reset_var_gen = modify (const init_cg)

next_var :: CGState Var
next_var = do
  s <- get
  let (v, g) = new_auto_var (var_gen s)
  put (s { var_gen = g
         , undeclared_vars = v : undeclared_vars s
         })
  return v

-- | Make new locally scoped (let) variables.
mk_let_vars :: forall a proxy. YulO1 a => proxy a -> CGState [Var]
mk_let_vars _ = reverse <$> go (abi_type_count_vars @a) []
  where go n vars = next_var >>= \var ->
          if n > 1 then go (n - 1) (var:vars) else return (var:vars)

-- | Assigning variables.
assign_vars_to_vars :: Indenter -> [Var] -> [Var] -> Code
assign_vars_to_vars ind varsTo varsFrom = gen_assert (length varsTo == length varsFrom) $
  T.intercalate "" (fmap (\(a,b) -> ind (a <> " := " <> b)) (zip varsTo varsFrom))

-- | Assigning expression @vals@ to variables @vars@.
assign_vals_to_vars :: Indenter -> [Var] -> [Val] -> Code
assign_vals_to_vars ind vars vals = gen_assert (length vars == length vals) $
  T.intercalate "" (fmap (\(a,b) -> ind (a <> " := " <> b)) (zip vars (fmap val_to_code vals)))

-- | Declare variables.
declare_vars :: CGState (Maybe Code)
declare_vars = do
  s <- get
  let vars = undeclared_vars s
      code = if null vars then Nothing else Just ("let " <> T.intercalate ", " vars)
  put (s { undeclared_vars = [] })
  return code

forget_vars :: CGState ()
forget_vars = modify $ \s -> s { undeclared_vars = [] }

mk_code :: forall a b. YulO2 a b => Indenter -> [Val] -> T.Text -> Proxy a -> Proxy b -> Code -> Code
mk_code ind vals title _ _ code = ind (
  "//dbg: +" <> title <> "(" <>
  vals_to_code vals <>
  " : "  <> T.pack (abiTypeCompactName @a) <>
  ") -> " <> T.pack (abiTypeCompactName @b)
  ) <>
  code <>
  ind ("//dbg: -" <> title)

-- coerce_vals :: forall a b. YulO2 a b => Indenter -> Proxy a -> Proxy b -> [Val] -> Code
-- coerce_vals ind pa pb vars = case (typeRep pa, typeRep pb) of
--   (ta, tb) | ta == tb -> ""
--            | otherwise -> ""

gen_code :: State CGStateData Code -> Code
gen_code x = evalState x init_cg


abi_type_count_vars :: forall a. ABITypeable a => Int
abi_type_count_vars = length (abiTypeInfo @a)

gen_assert :: HasCallStack => Bool -> a -> a
gen_assert = gen_assert_msg "codegen assertion!"

gen_assert_msg :: HasCallStack => String -> Bool -> a -> a
gen_assert_msg msg False _ = error msg
gen_assert_msg _     _ x   = x
