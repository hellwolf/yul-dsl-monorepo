module YulDSL.CodeGens.Yul.Internal.BuiltInRegistra
  ( BuiltInEntry, BuiltInRegistra
  , register_builtin, lookup_builtin
  , mk_builtin, const_builtin
  ) where

-- base
import           Data.List                                   (isPrefixOf)
-- text
import qualified Data.Text.Lazy                              as T
-- containers
import qualified Data.Map.Lazy                               as Map
--
import           YulDSL.CodeGens.Yul.Internal.CodeFormatters


type BuiltInName = String

type BuiltInPrefix = String

newtype PrefixKey = MkPrefixKey { unPrefixKey :: BuiltInPrefix }

instance Eq PrefixKey where
  (MkPrefixKey a) == (MkPrefixKey b) = isPrefixOf a b || isPrefixOf b a

instance Ord PrefixKey where
  a'@(MkPrefixKey a) <= b'@(MkPrefixKey b) = a' == b' || a <= b

-- A function that returns a builtin's code and its dependent built-ins.
type BuiltInYulGen = BuiltInName -> (T.Text, [BuiltInName])

-- The registra of builtin yul code generator.
type BuiltInRegistra = Map.Map PrefixKey BuiltInYulGen

type BuiltInEntry = (BuiltInPrefix, BuiltInYulGen)

register_builtin :: BuiltInEntry -> BuiltInRegistra -> BuiltInRegistra
register_builtin (prefix, gen) registra =
  let pkey = MkPrefixKey prefix
      go = Map.insert pkey gen registra
  in case Map.lookupLE pkey registra of
  Nothing         -> go
  Just (pkey', _) -> if pkey /= pkey' then go
                     else error ("Builtin registra prefix already exists: " <> unPrefixKey pkey')

lookup_builtin :: BuiltInName -> BuiltInRegistra -> (T.Text, [BuiltInName])
lookup_builtin name registra = case Map.lookupLE (MkPrefixKey name) registra of
  Nothing  -> error ("No builtin found: " <> name)
  Just (MkPrefixKey prefix, gen) -> if isPrefixOf prefix name then gen name
                                    else error ("Builtin not found: " <> name)

mk_builtin :: BuiltInPrefix
           -> (String {- partial name: builtin suffix -} -> BuiltInYulGen)
           -> BuiltInEntry
mk_builtin prefix g = (prefix, \n -> g (drop (length prefix) n) n)

const_builtin :: BuiltInName -> Code -> BuiltInEntry
const_builtin name code = (name, const (code, []))
