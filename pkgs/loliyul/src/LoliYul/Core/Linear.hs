{-# LANGUAGE LinearTypes           #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UnicodeSyntax         #-}

module LoliYul.Core.Linear where

import qualified Data.Text                    as T

import           Control.Category.Constrained (Cartesian, Category (Obj), O2,
                                               O4, type (⊗))
import           Control.Category.Linear      (P, copy, decode, discard, encode,
                                               ignore, merge, split, unit)

import           LoliYul.Core.Types
import           LoliYul.Core.YulCat


--------------------------------------------------------------------------------
-- Extra SMC Linear Combinators
--------------------------------------------------------------------------------

id1 :: forall k con r a.
       ( Cartesian k {-<-}, O2 k r a, con ~ Obj k {->-}
       , con (), (forall α β. (con α, con β) => con (α,β))
       ) => P k r a ⊸ P k r a
id1 = ignore unit

copy' :: forall k con r a.
         ( Cartesian k {-<-}, O2 k r a, con ~ Obj k {->-}
         , con (), (forall α β. (con α, con β) => con (α,β))
         ) => P k r a ⊸ (P k r a, P k r a)
copy' a = copy a & split

copyAp :: forall k con r a b c.
          ( Cartesian k {-<-}, O4 k r a b c, con ~ Obj k {->-}
          , con (), (forall α β. (con α, con β) => con (α,β))
          ) => P k r a ⊸ (P k r a ⊸ P k r b) ⊸ (P k r a ⊸ P k r c) ⊸ P k r (b⊗c)
copyAp a f1 f2 = copy a & split & \(a1, a2) -> merge (f1 a1, f2 a2)

copyAp' :: forall k con r a b c.
           ( Cartesian k {-<-}, O4 k r a b c, con ~ Obj k {->-}
           , con (), (forall α β. (con α, con β) => con (α,β))
           ) => P k r a ⊸ (P k r a ⊸ P k r b) ⊸ (P k r a ⊸ P k r c) ⊸ (P k r b, P k r c)
copyAp' a f1 f2 = copy a & split & \(a1, a2) -> (f1 a1, f2 a2)

--------------------------------------------------------------------------------
-- YulCat Linear Combinators
--------------------------------------------------------------------------------

-- | Polymorphic port type for linear function APIs of YulCat
type YulPort r a = P YulCat r a

-- TODO, use linear-base fst
fst1 :: forall a b r. (YulCon r, YulCon a, YulCon b)
     => (YulPort r a ⊗ YulPort r b) ⊸ YulPort r a
fst1 (a, b) = encode YulUnitorR (merge (a, discard b))

-- TODO use linear-ase
-- instance YulNum a => Num (YulPort r a) where
(+:) :: (YulCon r, YulNum a) => YulPort r a ⊸ YulPort r a ⊸ YulPort r a
a +: b = encode YulNumPlus (merge (a,b))

(-:) :: (YulCon r, YulNum a) => YulPort r a ⊸ YulPort r a ⊸ YulPort r a
a -: b = encode YulNumMinus (merge (a,b))

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
sget = encode YulSGet

sput :: forall v r. (YulCon r, YulVal v)
     => YulPort r YulAddr ⊸ YulPort r v ⊸ YulPort r ()
sput toP valP = encode YulSPut (merge (toP, valP))
(<==) :: forall v r. (YulCon r, YulVal v)
      => YulPort r YulAddr ⊸ YulPort r v ⊸ YulPort r ()
(<==) = sput

sputAt :: forall v r. (YulCon r, YulVal v)
       => YulAddr -> YulPort r v ⊸ YulPort r ()
sputAt to = yulConst to unit & sput
(<=@) :: forall v r. (YulCon r, YulVal v)
      => YulAddr -> YulPort r v ⊸ YulPort r ()
(<=@) = sputAt

infixr 1 <==, <=@

defun :: T.Text
      -> (forall r. YulCon r => YulPort r [YulType] ⊸ YulPort r [YulType])
      -> YulFunction
defun name f = YulFunc name (decode f)
