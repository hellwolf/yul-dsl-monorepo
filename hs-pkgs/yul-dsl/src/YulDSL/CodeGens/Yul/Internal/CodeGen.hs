{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings   #-}
module YulDSL.CodeGens.Yul.Internal.CodeGen
  ( -- $codegen_state
    CGState
  , gen_code
    -- $codegen_vars
  , cg_reset_var_gen
  , cg_mk_let_vars
  , cg_declare_vars
  , cg_forget_vars
    -- $codegen_dependencies
  , cg_list_dependent_cats
  , cg_insert_dependent_cat
    -- $codegen_builtins
  , cg_register_builtin
  , cg_use_builtin
  , cg_gen_builtin_codes
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


-- $codegen_state
-- == CodeGen State

-- | CodeGen state data.
data CGStateData = MkCGStateData
  { var_gen         :: AutoVarGen
  , undeclared_vars :: [Var]
  , dependent_cats  :: Map.Map String AnyYulCat -- cat_id -> cat
  , builtins        :: BuiltInRegistra
  , builtin_used    :: Set.Set String
  }

-- | CodeGen state.
type CGState = State CGStateData

-- | Initial CodeGen state data.
init_cg_state_data :: CGStateData
init_cg_state_data = MkCGStateData
  { var_gen = MkAutoVarGen 0
  , undeclared_vars = []
  , builtins = Map.empty
  , dependent_cats = Map.empty
  , builtin_used = Set.empty
  }

-- | Generate code from the initial CodeGen state.
gen_code :: CGState Code -> Code
gen_code s = evalState s init_cg_state_data

-- $codegen_vars
-- == CodeGen Variables

-- | Reset the variable generator.
cg_reset_var_gen :: CGState ()
cg_reset_var_gen = modify $ \s -> s { var_gen = MkAutoVarGen 0 }

-- | Generate the next variable name.
cg_next_var :: CGState Var
cg_next_var = do
  s <- get
  let (v, g) = new_auto_var (var_gen s)
  put (s { var_gen = g
         , undeclared_vars = v : undeclared_vars s
         })
  return v

-- | Make new locally scoped (let) variables needed for the type @a@.
cg_mk_let_vars :: forall a. YulO1 a => CGState [Var]
cg_mk_let_vars = reverse <$> go (abi_type_count_vars @a) []
  where go 0 vars = pure vars
        go n vars = cg_next_var >>= \var -> go (n - 1) (var:vars)

-- | Declare the undeclared variables.
cg_declare_vars :: CGState (Maybe Code)
cg_declare_vars = do
  s <- get
  let vars = undeclared_vars s
      code = if null vars then Nothing else Just ("let " <> T.intercalate ", " vars)
  put (s { undeclared_vars = [] })
  return code

-- | Forget about the undeclared variables.
cg_forget_vars :: CGState ()
cg_forget_vars = modify $ \s -> s { undeclared_vars = [] }

-- $codegen_dependencies
-- == CodeGen YulCat Dependencies

cg_list_dependent_cats :: CGState [(String, AnyYulCat)]
cg_list_dependent_cats = get <&> Map.toList . dependent_cats

cg_insert_dependent_cat :: String -> AnyYulCat -> CGState ()
cg_insert_dependent_cat depId depCat = modify
  (\d@(MkCGStateData { dependent_cats = deps }) -> d { dependent_cats = Map.insert depId depCat deps })

-- $codegen_builtins
-- == CodeGen Builtins

cg_register_builtin :: BuiltInEntry -> CGState ()
cg_register_builtin builtin = modify $ \s -> s { builtins = register_builtin builtin s.builtins }

cg_use_builtin :: String -> CGState ()
cg_use_builtin name = modify
  (\d@(MkCGStateData { builtin_used }) -> d { builtin_used = Set.insert name builtin_used })

cg_gen_builtin_codes :: Indenter -> CGState [Code]
cg_gen_builtin_codes ind = get >>= \(MkCGStateData{ builtins , builtin_used }) ->
  let allBuiltIns = closure (\x -> x : snd (lookup_builtin x builtins)) builtin_used
  in pure $
     filter (not . T.null) $ -- some built-ins are built-in of yul language, hence with empty extra code.
     map (\x -> fst (lookup_builtin x builtins) ind) (Set.toList allBuiltIns)
  where closure f s0 = go (Set.toList s0) s0
          where go xs s =
                  let xs' = concatMap f xs
                      s'  = Set.union s (Set.fromList xs')
                  in if Set.size s == Set.size s' then s
                  else go (Set.toList (Set.difference s' s)) s'
