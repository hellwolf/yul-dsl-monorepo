module YulDSL.CodeGens.Yul.Internal.BuiltInRegistra
  ( BuiltInYulGen, BuiltInRegistra, register_builtin, lookup_builtin
  ) where

-- base
import           Data.List      (isPrefixOf)
-- text
import qualified Data.Text.Lazy as T
-- containers
import qualified Data.Map.Lazy  as Map


newtype PrefixKey = MkPrefixKey { unPrefixKey :: String }

instance Eq PrefixKey where
  (MkPrefixKey a) == (MkPrefixKey b) = isPrefixOf a b || isPrefixOf b a

instance Ord PrefixKey where
  a'@(MkPrefixKey a) <= b'@(MkPrefixKey b) = a' == b' || a <= b

type BuiltInYulGen = String -> T.Text

type BuiltInRegistra = Map.Map PrefixKey BuiltInYulGen

register_builtin :: String -> BuiltInYulGen -> BuiltInRegistra -> BuiltInRegistra
register_builtin prefix gen registra =
  let pkey = MkPrefixKey prefix
      go = Map.insert pkey gen registra
  in case Map.lookupLE pkey registra of
  Nothing         -> go
  Just (pkey', _) -> if pkey /= pkey' then go
                     else error ("Builtin registra prefix existed: " <> unPrefixKey pkey')

lookup_builtin :: String -> BuiltInRegistra -> T.Text
lookup_builtin name registra = case Map.lookup (MkPrefixKey name) registra of
  Nothing  -> error ("No builtin found: " <> name)
  Just gen -> gen name
