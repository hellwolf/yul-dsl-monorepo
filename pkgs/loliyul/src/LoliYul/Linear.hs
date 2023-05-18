{-# LANGUAGE LinearTypes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}

module LoliYul.Linear where

import           Data.Proxy                   (Proxy (..))
import qualified Data.Text                    as T

import           Control.Category.Constrained (type (⊗))
import           Control.Category.Linear      (P, decode, discard, encode,
                                               ignore, merge, split, unit)

import           LoliYul.Types
import           LoliYul.YulCat


-- | Polymorphic port type for linear function APIs of YulCat
type YulPort r a = P YulCat r a

-- TODO use linear-ase
-- instance YulNum a => Num (YulPort r a) where
(+:) :: (YulCon r, YulNum a) => YulPort r a ⊸ YulPort r a ⊸ YulPort r a
a +: b = (encode YulNumPlus) (merge (a,b))

(-:) :: (YulCon r, YulNum a) => YulPort r a ⊸ YulPort r a ⊸ YulPort r a
a -: b = (encode YulNumMinus) (merge (a,b))

-- TODO, use linear-base fst
fst1 :: forall a b r. (YulCon r, YulCon a, YulCon b)
           => (YulPort r a ⊗ YulPort r b) ⊸ YulPort r a
fst1 (a, b) = encode YulUnitorR (merge (a, discard b))

-- | The Linear function pipeline builder from left to right.
{-# INLINE (&) #-}
(&) :: forall a b m. a %m -> (a %m -> b) %m -> b
x & f = f x

yulConst :: forall r a b. (YulCon r, YulCon a, YulCon b)
         => b -> (YulPort r a ⊸ YulPort r b)
yulConst b = encode (YulConst b)

yulJust :: forall a r. (YulCon r, YulCon a)
       => YulPort r (Maybe a) ⊸ YulPort r a
yulJust = encode YulJust

abiPop :: forall a r. (YulVal a, YulCon r)
       => YulPort r [YulType] ⊸ YulPort r a
abiPop a = encode YulDetuple a & split & fst1 & yulJust

abiIter :: forall a r. (YulVal a, YulCon r)
        => YulPort r [YulType] ⊸ (YulPort r a, YulPort r [YulType])
abiIter a = encode YulDetuple a & split & \(a, b) -> merge (yulJust a, b) & split

-- TODO actual abi encoding needed
abiReturn :: forall r a b. (YulCon r, YulCon a, YulCon b)
          => b -> (YulPort r a ⊸ YulPort r b)
abiReturn = yulConst

sget :: forall v r. (YulCon r, YulVal v)
     => (YulPort r YulAddr ⊸ YulPort r v)
sget atP = encode YulSGet atP

sput :: forall v r. (YulCon r, YulVal v)
     => YulPort r YulAddr ⊸ YulPort r v ⊸ YulPort r ()
sput toP valP = encode YulSPut (merge (toP, valP))
(<==) :: forall v r. (YulCon r, YulVal v)
      => YulPort r YulAddr ⊸ YulPort r v ⊸ YulPort r ()
(<==) = sput

sputAt :: forall v r. (YulCon r, YulVal v)
       => YulAddr -> YulPort r v ⊸ YulPort r ()
sputAt to valP = sput (yulConst to unit) valP
(<=@) :: forall v r. (YulCon r, YulVal v)
      => YulAddr -> YulPort r v ⊸ YulPort r ()
(<=@) = sputAt

infixr 1 <==, <=@

defun :: T.Text
      -> (forall r. YulCon r => YulPort r [YulType] ⊸ YulPort r [YulType])
      -> YulFunction
defun name f = YulFunc name (decode f)
