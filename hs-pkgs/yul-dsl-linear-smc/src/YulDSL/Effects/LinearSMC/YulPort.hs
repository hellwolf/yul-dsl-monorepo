module YulDSL.Effects.LinearSMC.YulPort
  ( -- $linear_port_defs
    PortEffect (PurePort, VersionedPort)
  , P'x, P'V, P'P
    -- $general_ops
  , emb'l, const'l, dup2'l
    -- $abi_ops
  , coerce'l, cons'l, uncons'l
  ) where
-- linear-base
import           Prelude.Linear
-- linear-smc
import           Control.Category.Linear
-- yul-dsl
import           YulDSL.Core
--
import           Control.Category.Constrained.YulDSL ()
import           Data.MPOrd


-- $linear_port_defs
-- = Yul Port Definitions

-- | Various types of port effects for the yul port API.
data PortEffect = PurePort          -- ^ Pure port that does not need to be versioned
                | VersionedPort Nat -- ^ Linearly versioned port

type instance NonPureEffect PurePort = False
type instance NonPureEffect (VersionedPort v) = True

-- | Linear port of yul categories with the port effect kind, aka. yul ports.
type P'x (eff :: PortEffect) = P (YulCat eff)

-- | Linear port of yul category with pure data, aka. pure yul ports.
type P'P = P'x PurePort

-- | Linear port of yul category with linearly versioned data, aka. versioned yul ports.
type P'V v = P'x (VersionedPort v)

-- $ general_ops
-- = General Yul Port Operations
--
-- Note: Yul ports are defined above as "P'*", and a "yul port diagram" is a linear function from input yul port to a
-- output yul port.

-- | Embed a free value to a yul port diagram that discards any input yul ports.
emb'l :: forall a b eff r. YulO3 a b r => a -> (P'x eff r b ⊸ P'x eff r a)
emb'l a = encode (YulEmb a) . discard

-- | Create a constant yul port diagram that discards any input yul ports.
const'l :: forall a b eff r. (YulO3 a b r) => P'x eff r a ⊸ (P'x eff r b ⊸ P'x eff r a)
const'l = flip (ignore . discard)

-- | Duplicate the input yul port twice as a tuple.
dup2'l :: forall a eff r. YulO2 a r => P'x eff r a ⊸ (P'x eff r a, P'x eff r a)
dup2'l = split . copy

-- $abi_ops
-- = 'ABITypeable' Specific Operations

-- | Coerce input yul port to an ABI coercible output yul port.
coerce'l :: forall a b eff r. (YulO3 a b r, ABITypeCoercible a b) => P'x eff r a ⊸ P'x eff r b
coerce'l = encode YulCoerceType

-- | Prepend an element to a 'NP'.
cons'l :: forall x xs eff r. YulO3 x (NP xs) r => P'x eff r x ⊸ P'x eff r (NP xs) ⊸ P'x eff r (NP (x:xs))
cons'l x xs = coerce'l (merge (x, xs))

-- | Split a 'NP' into its first element and the rest.
uncons'l :: forall x xs eff r. YulO3 x (NP xs) r => P'x eff r (NP (x:xs)) ⊸ (P'x eff r x, P'x eff r (NP xs))
uncons'l = split . coerce'l


--
-- 'MPEq' instance for the yul ports.
--

instance (YulO1 r, YulNum a) => MPEq (P'x eff r a) (P'x eff r BOOL) where
  a == b = encode yulNumEq (merge (a, b))
  a /= b = encode yulNumNe (merge (a, b))

-- | 'MPOrd' instance for the yul ports.
instance (YulO1 r, YulNum a) => MPOrd (P'x eff r a) (P'x eff r BOOL) where
  a  < b = encode yulNumLt (merge (a, b))
  a <= b = encode yulNumLe (merge (a, b))
  a  > b = encode yulNumGt (merge (a, b))
  a >= b = encode yulNumGe (merge (a, b))

--
-- Num instances for (P'V v r)
--

instance (YulNum a, YulO1 r) => Additive (P'V v r a) where
  a + b = encode (jmpBuiltIn (yulNumAdd @a)) (merge (a, b))

instance (YulNum a, YulO1 r) => AddIdentity (P'V v r a) where
  -- Note: uni-port is forbidden in linear-smc, but linear-base AdditiveGroup requires this instance.
  zero = error "unit is undefined for linear ports"

instance (YulNum a, YulO1 r) => AdditiveGroup (P'V v r a) where
  negate = encode (jmpBuiltIn (yulNumNeg @a))

--
-- 'IfThenElse' instance for the yul ports.
--

-- FIXME, this should use pattern matching and built-in Bool type; otherwise the linearity of the branches are not
-- respected.
instance YulO2 a r => IfThenElse (P'x eff r BOOL) (P'x eff r a) where
  ifThenElse c a b = encode YulITE (merge(c, merge(a, b)))
