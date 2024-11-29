module YulDSL.Effects.LinearSMC.LinearThread
  ( LT, runLT, fin'with, fin'dis, fin'emb, fin'const
  , (|>), (|=)
  , sget, sput, sputAt
  ) where

-- base
import           GHC.TypeLits                        (type (+), type (<=))
-- linear-base
import           Control.Category.Linear
import           Prelude.Linear
import qualified Unsafe.Linear                       as UnsafeLinear
-- yul-dsl
import           YulDSL.Core
--
import           YulDSL.Effects.LinearSMC.LinearPort

data LT (v :: Nat) r a = MkLT (P'V v r a)
runLT :: forall v r a. YulO2 a r => LT v r a ⊸ P'V v r a
runLT (MkLT x) = x

(|>) :: forall a b r v1 vn. ( YulO3 a b r -- , v1 + 1 <= vn
                            ) => LT v1 r a ⊸ (P'V v1 r a ⊸ LT vn r b) ⊸ LT vn r b
(|>) (MkLT x) f = f x

(|=) :: forall a b r v. ( YulO3 a b r
                        ) => LT v r a ⊸ (P'V v r a ⊸ LT v r b) ⊸ LT v r b
(|=) (MkLT x) f = f x

infixl 1 |>, |=

fin'with :: forall v r a. YulO2 a r => P'V v r a ⊸ LT v r a
fin'with = MkLT

fin'dis :: forall v r a. YulO2 a r => P'V v r a ⊸ LT v r ()
fin'dis = MkLT . discard

fin'emb :: forall v r a b. YulO3 a b r => a -> (P'V v r b ⊸ LT v r a)
fin'emb a b = MkLT (emb'l a b)

fin'const :: forall v r a b. YulO3 a b r =>  P'V v r a -> (P'V v r b ⊸ LT v r a)
fin'const a b = MkLT (const'l a b)

sget :: forall a r v. (YulO2 a r, ABIWordValue a)
     => P'V v r ADDR ⊸ LT v r a
sget a = MkLT (encode YulSGet a)

sput :: forall a r v. (YulO2 a r, ABIWordValue a)
     => P'V v r ADDR ⊸ P'V v r a ⊸ LT (v + 1) r a
sput to x =
  dup2'l x
  & \(x1, x2) -> encode YulSPut (merge (to, x1))
  & \u -> MkLT (UnsafeLinear.coerce (ignore u x2))

sputAt :: forall a r v. (YulO2 a r, ABIWordValue a)
       => ADDR -> P'V v r a ⊸ LT (v + 1) r a
sputAt to x = mkUnit x & \(x', u) -> emb'l to u & \a -> sput a x'
