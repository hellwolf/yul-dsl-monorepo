module Data.TupleN
  ( module Data.TupleN.TH
  , Solo (MkSolo)
  , ConvertibleTupleN
  ) where

-- ghc-experimental
import           Data.Tuple.Experimental (Solo (MkSolo))
--
import           Data.TupleN.TH


type ConvertibleTupleN tpl = ( NPtoTupleN (TupleNtoNP tpl) ~ tpl
                             , FromTupleNtoNP tpl
                             , FromNPtoTupleN (TupleNtoNP tpl)
                             )
