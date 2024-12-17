{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings   #-}
module YulDSL.CodeGens.Yul.Internal.CodeGen
  ( CGState, CGOutput
  , cg_reset_var_gen
  , cg_mk_let_vars
  , cg_declare_vars
  , cg_forget_vars
  , cg_list_dependent_cats
  , cg_insert_dependent_cat
  , cg_register_builtin
  , cg_use_builtin
  , cg_gen_builtin_codes
  , gen_code
  ) where

-- base
import           Control.Monad.State.Lazy                     (MonadState (..), State, evalState, modify)
import           Data.Functor                                 ((<&>))
-- text
import qualified Data.Text.Lazy                               as T
-- containers
import qualified Data.Map.Lazy                                as Map
import qualified Data.Set                                     as Set
--
import           YulDSL.Core
--
import           YulDSL.CodeGens.Yul.Internal.BuiltInRegistra
import           YulDSL.CodeGens.Yul.Internal.CodeFormatters
import           YulDSL.CodeGens.Yul.Internal.Variables


------------------------------------------------------------------------------------------------------------------------
-- CodeGen (CG) Machinery
------------------------------------------------------------------------------------------------------------------------

-- | CodeGen state data.
data CGStateData = MkCGStateData
  { var_gen         :: AutoVarGen
  , undeclared_vars :: [Var]
  , dependent_cats  :: Map.Map String AnyYulCat -- cat_id -> cat
  , builtins        :: BuiltInRegistra
  , builtin_used    :: Set.Set String
  }

init_cg_state_data :: CGStateData
init_cg_state_data = MkCGStateData
  { var_gen = MkAutoVarGen 0
  , undeclared_vars = []
  , builtins = Map.empty
  , dependent_cats = Map.empty
  , builtin_used = Set.empty
  }

type CGState = State CGStateData

type CGOutput = (Code, [Val])

cg_reset_var_gen :: CGState ()
cg_reset_var_gen = modify $ \s -> s { var_gen = MkAutoVarGen 0 }

cg_next_var :: CGState Var
cg_next_var = do
  s <- get
  let (v, g) = new_auto_var (var_gen s)
  put (s { var_gen = g
         , undeclared_vars = v : undeclared_vars s
         })
  return v

-- | Make new locally scoped (let) variables.
cg_mk_let_vars :: forall a. YulO1 a => CGState [Var]
cg_mk_let_vars = reverse <$> go (abi_type_count_vars @a) []
  where go n vars = cg_next_var >>= \var ->
          if n > 1 then go (n - 1) (var:vars) else return (var:vars)

-- | Declare variables.
cg_declare_vars :: CGState (Maybe Code)
cg_declare_vars = do
  s <- get
  let vars = undeclared_vars s
      code = if null vars then Nothing else Just ("let " <> T.intercalate ", " vars)
  put (s { undeclared_vars = [] })
  return code

cg_forget_vars :: CGState ()
cg_forget_vars = modify $ \s -> s { undeclared_vars = [] }

cg_register_builtin :: (String, BuiltInYulGen) -> CGState ()
cg_register_builtin (prefix, gen) = modify $ \s -> s { builtins = register_builtin prefix gen s.builtins }

cg_list_dependent_cats :: CGState [(String, AnyYulCat)]
cg_list_dependent_cats = get <&> Map.toList . dependent_cats

cg_insert_dependent_cat :: String -> AnyYulCat -> CGState ()
cg_insert_dependent_cat depId depCat = modify
  (\d@(MkCGStateData { dependent_cats = deps }) -> d { dependent_cats = Map.insert depId depCat deps })

cg_use_builtin :: String -> CGState ()
cg_use_builtin name = modify
  (\d@(MkCGStateData { builtin_used }) -> d { builtin_used = Set.insert name builtin_used })

cg_gen_builtin_codes :: CGState [Code]
cg_gen_builtin_codes = get >>= \(MkCGStateData{ builtins , builtin_used }) ->
  pure $ map (\x -> lookup_builtin x builtins) (Set.toList builtin_used)

gen_code :: CGState Code -> Code
gen_code s = evalState s init_cg_state_data
