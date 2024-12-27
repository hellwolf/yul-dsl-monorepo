module YulDSL.CodeGens.Yul.Internal.BuiltInRegistra
  ( BuiltInName, BuiltInPrefix
    -- $builtin_registra
  , BuiltInRegistra
    -- $builtin_registra_operations
  , BuiltInEntry, register_builtin, lookup_builtin
    -- $builtin_builders
  , mk_builtin, const_builtin, const_builtin_with_deps
  ) where

-- base
import Data.List                                   (isPrefixOf)
-- text
import Data.Text.Lazy                              qualified as T
-- containers
import Data.Map.Lazy                               qualified as Map
--
import YulDSL.CodeGens.Yul.Internal.CodeFormatters


-- | The type alias for names of built-ins.
type BuiltInName = String

-- | The type alias for prefixes of built-ins.
type BuiltInPrefix = String

-- $builtin_registra
-- == Built-in Registra
--
-- Builtin registra is a map of 'PrefixKey' to 'BuiltInYulGen'.

newtype PrefixKey = MkPrefixKey { unPrefixKey :: BuiltInPrefix }

instance Eq PrefixKey where
  (MkPrefixKey a) == (MkPrefixKey b) = isPrefixOf a b || isPrefixOf b a

instance Ord PrefixKey where
  a'@(MkPrefixKey a) <= b'@(MkPrefixKey b) = a' == b' || a <= b

-- A function that returns a builtin's code and its dependent built-ins.
type BuiltInYulGen = BuiltInName -> (Indenter -> T.Text, [BuiltInName])

-- | The registra of builtin yul code generator.
type BuiltInRegistra = Map.Map PrefixKey BuiltInYulGen

-- | $builtin_registra_operations
-- == Built-in Registra Operations

-- | Entry data for registering a new builtin.
type BuiltInEntry = (BuiltInPrefix, BuiltInYulGen)

register_builtin :: BuiltInEntry -> BuiltInRegistra -> BuiltInRegistra
register_builtin (prefix, gen) registra =
  let pkey = MkPrefixKey prefix
      go = Map.insert pkey gen registra
  in case Map.lookupLE pkey registra of
  Nothing         -> go
  Just (pkey', _) -> if pkey /= pkey' then go
                     else error ("Builtin registra prefix already exists: " <> unPrefixKey pkey')

lookup_builtin :: BuiltInName -> BuiltInRegistra -> (Indenter -> T.Text, [BuiltInName])
lookup_builtin name registra = case Map.lookupLE (MkPrefixKey name) registra of
  Nothing  -> error ("No builtin found: " <> name)
  Just (MkPrefixKey prefix, gen) -> if prefix `isPrefixOf` name then gen name
                                    else error ("Builtin not found: " <> name)

-- $builtin_builders
-- == Built-in Builders

mk_builtin :: BuiltInPrefix
           -> (String {- partial name / builtin suffix -} -> BuiltInName -> ([Code], [BuiltInName]))
           -> BuiltInEntry
mk_builtin prefix g = (prefix, \fullName -> let (codeLines, deps) = g (suffixOf fullName) fullName
                                            in (\ind -> foldr (T.append . ind) T.empty codeLines, deps))
  where suffixOf = drop (length prefix)

const_builtin :: BuiltInName -> [Code] -> BuiltInEntry
const_builtin name codeLines = mk_builtin name (\_ _ -> (codeLines, []))

const_builtin_with_deps :: BuiltInName -> [Code] -> [BuiltInName] -> BuiltInEntry
const_builtin_with_deps name codeLines deps = mk_builtin name (\_ _ -> (codeLines, deps))
