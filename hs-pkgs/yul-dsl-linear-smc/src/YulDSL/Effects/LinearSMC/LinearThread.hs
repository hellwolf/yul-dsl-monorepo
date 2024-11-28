module YulDSL.Effects.LinearSMC.LinearThread where

-- sget :: forall a r v. (YulO2 a r, ABIWordValue a)
--      => P'L v r ADDR ⊸ P'L v r a
-- sget = encode YulSGet

-- sput :: forall a b r v1 vd. (YulO3 a b r, ABIWordValue a, v1 <= vd, v1 + 1 <= vd)
--      => P'L v1 r ADDR ⊸ P'L v1 r a ⊸ (P'L (v1 + 1) r a ⊸ P'L vd r b) ⊸ P'L vd r b
-- sput to x f =
--   use'l x (\x1 -> encode YulSPut (merge (to, x1)))
--   -- increase port version by one
--   &+ \x2 u -> UnsafeLinear.coerce @(P'L v1 r a) @(P'L (v1 + 1) r a) (ignore u x2)
--   & f

-- sputAt :: forall a b r v1 vd. (YulO3 a b r, ABIWordValue a, v1 <= vd, v1 + 1 <= vd)
--        => ADDR -> P'L v1 r a ⊸ (P'L (v1 + 1) r a ⊸ P'L vd r b) ⊸ P'L vd r b
-- sputAt to x f = mkUnit x & \(x', u) -> emb'l to u & \a -> sput a x' f
