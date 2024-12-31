{-# LANGUAGE LinearTypes #-}
{-|
Copyright   : (c) 2023-2024 Miao, ZhiCheng
License     : LGPL-3
Maintainer  : hellwolf@yolc.dev
Stability   : experimental

= Description

[Yul](https://docs.soliditylang.org/en/latest/yul.html) is an intermediate language that is part of the [solidity
compiler](https://docs.soliditylang.org/en/latest/). It is by-design aspiring to be compiled to bytecode for different
backends, while at the moment it is for [Ethereum Virtual Machine](https://ethereum.org/en/developers/docs/evm/) (EVM).

This module provides an "Embedded (in Haskell) Domain Specific Language" (eDSL) for programming in Yul, called 'YulCat'.

YulCat is based on category theory. The objects in this category are instances of 'YulCatObj'.

Further more, the 'YulCat' is instantiated as a "Symmetric Monoidal Category" (SMC). Being a SMC enables the possibility
for compiling linearly-typed functions in Haskell directly to the 'YulCat', where linear-types can provide additional
safety to the practice of EVM programming.

-}
module YulDSL.Core.YulCat
  ( NonPureEffect, IsPureEffect, IsNonPureEffect
  , NamedYulCat
  , YulCat (..), AnyYulCat (..), Fn (MkFn, unFn)
  , (>.>), (<.<)
  , yulNumLt, yulNumLe, yulNumGt, yulNumGe, yulNumEq, yulNumNe
  , yulRevert, yulKeccak256
  , Referenceable (yulRefGet, yulRefPut)
  , yulCatCompactShow, yulCatFingerprint
  , module Control.IfThenElse
  , module Control.PatternMatchable
  , PatternMatchableYulCat
  ) where

-- base
import Data.Char                (ord)
import Data.Kind                (Constraint)
import Data.Proxy               (Proxy (Proxy))
import GHC.Integer              (xorInteger)
import GHC.TypeError            (Assert, ErrorMessage (Text), TypeError)
import Text.Printf              (printf)
-- bytestring
import Data.ByteString.Char8    qualified as B
-- eth-abi
import Ethereum.ContractABI
--
import Control.IfThenElse
import Control.PatternMatchable
--
import YulDSL.Core.YulCatObj
import YulDSL.Core.YulNum


------------------------------------------------------------------------------------------------------------------------
-- The Cat
------------------------------------------------------------------------------------------------------------------------

-- | An open type family for marking effects non-pure, in order to access some restricted YulCat morphisms.
type family NonPureEffect (eff :: k) :: Bool

-- | Test if an effect can be used for pure morphisms.
type IsPureEffect :: k -> Constraint
type IsPureEffect eff = Assert (Not (NonPureEffect eff))
                        (TypeError (Text "pure effect expected"))

-- | Test if an effect can be used for non pure morphisms.
type IsNonPureEffect :: k -> Constraint
type IsNonPureEffect eff = Assert (NonPureEffect eff)
                           (TypeError (Text "non-pure effect expected"))

-- -- | An open type family for marking effects non-pure, in order to access some restricted YulCat morphisms.
-- type family NonStaticEffect (eff :: k) :: Bool

-- | Named YulCat morphism.
type NamedYulCat eff a b = (String, YulCat eff a b)

-- | A GADT-style DSL of Yul that constructs morphisms between objects (YulCatObj) of the "Yul Category".
--
--  Note: Unlike its moniker name "Cat" may suggest, the constructors of this data type are morphisms of the Yul
--  category.
data YulCat (eff :: k) a b where
  -- * Type Conversions
  --
  -- ^ Convert from extended yul object to its core yul object.
  YulReduceType :: forall eff a b. (YulO2 a b, ABITypeDerivedOf a ~ b) => YulCat eff a b
  -- ^ Extend core yul object type.
  YulExtendType :: forall eff a b. (YulO2 a b, a ~ ABITypeDerivedOf b) => YulCat eff a b
  -- ^ Convert between coercible yul objects.
  YulCoerceType :: forall eff a b. (YulO2 a b, ABITypeCoercible a b) => YulCat eff a b
  -- ^ Split the head and tail of a n-ary product where n >= 1.
  YulSplit :: forall eff xs. YulO1 (NP xs) => YulCat eff (NP xs) (Head xs, NP (Tail xs))

  -- * SMC Primitives
  --
  -- ** Category
  YulId   :: forall eff a.     YulO2 a a   => YulCat eff a a
  YulComp :: forall eff a b c. YulO3 a b c => YulCat eff c b %1-> YulCat eff a c %1-> YulCat eff a b
  -- ** Monoidal Category
  YulProd :: forall eff a b c d. YulO4 a b c d => YulCat eff a b %1-> YulCat eff c d %1-> YulCat eff (a, c) (b, d)
  YulSwap :: forall eff a b.     YulO2 a b     => YulCat eff (a, b) (b, a)
  -- ** Cartesian Category
  YulFork :: forall eff a b c. YulO3 a b c => YulCat eff a b %1-> YulCat eff a c %1-> YulCat eff a (b, c)
  YulExl  :: forall eff a b.   YulO2 a b   => YulCat eff (a, b) a
  YulExr  :: forall eff a b.   YulO2 a b   => YulCat eff (a, b) b
  YulDis  :: forall eff a.     YulO1 a     => YulCat eff a ()
  YulDup  :: forall eff a.     YulO1 a     => YulCat eff a (a, a)

  -- * Control Flow Primitives
  --
  -- ^ Embed a constant value @b@ and disregard any input object @a@.
  YulEmb :: forall eff a b. YulO2 a b => b %1-> YulCat eff a b
  -- ^ Jump to an user-defined morphism.
  YulJmpU :: forall eff a b. YulO2 a b => NamedYulCat eff a b %1-> YulCat eff a b
  -- ^ Jump to a built-in yul function
  YulJmpB :: forall eff a b. YulO2 a b => BuiltInYulFunction a b %1-> YulCat eff a b
  -- ^ If-then-else expression.
  YulITE :: forall eff a b. YulO2 a b => YulCat eff a b %1-> YulCat eff a b %1-> YulCat eff (BOOL, a) b

  -- * Storage Primitives
  --
  -- ^ Get storage word.
  YulSGet :: forall eff a. (IsNonPureEffect eff, YulO1 a, ABIWordValue a) => YulCat eff B32 a
  -- ^ Put storage word.
  YulSPut :: forall eff a. (IsNonPureEffect eff, YulO1 a, ABIWordValue a) => YulCat eff (B32, a) ()

-- | Existential wrapper of the 'YulCat'.
data AnyYulCat = forall eff a b. YulO2 a b => MkAnyYulCat (YulCat eff a b)

-- | Convenience operator for left to right composition of 'YulCat'.
(>.>) :: forall eff a b c. YulO3 a b c => YulCat eff a b %1-> YulCat eff b c %1-> YulCat eff a c
m >.> n = n `YulComp` m

-- | Convenience operator for right-to-left composition of 'YulCat'.
(<.<) :: forall eff a b c. YulO3 a b c => YulCat eff b c %1-> YulCat eff a b %1-> YulCat eff a c
(<.<) = YulComp

-- Same precedence as (>>>) (<<<);
-- see https://hackage.haskell.org/package/base-4.20.0.1/docs/Control-Category.html
infixr 1 >.>, <.<

-- | Yul function wrappers that are in currying function forms.
--
--   Note: Fn (a1 -> a2 -> ...aN -> b) ~ FnNP (NP [a1,a2...aN]) b
data Fn eff f where
  MkFn :: forall eff f xs b.
          ( UncurryNP'Fst f ~ xs,
            UncurryNP'Snd f ~ b,
            YulO2 (NP xs) b
          )
       => { unFn :: NamedYulCat eff (NP (UncurryNP'Fst f)) (UncurryNP'Snd f) } -> Fn eff f

------------------------------------------------------------------------------------------------------------------------
-- YulCat Pure "Verbs"
------------------------------------------------------------------------------------------------------------------------

-- Densely lined-up of YulNum Ord operations, to be used for creating number-related instances:
yulNumLt :: forall eff a. YulNumCmp a => YulCat eff (a, a) BOOL
yulNumLe :: forall eff a. YulNumCmp a => YulCat eff (a, a) BOOL
yulNumGt :: forall eff a. YulNumCmp a => YulCat eff (a, a) BOOL
yulNumGe :: forall eff a. YulNumCmp a => YulCat eff (a, a) BOOL
yulNumEq :: forall eff a. YulNumCmp a => YulCat eff (a, a) BOOL
yulNumNe :: forall eff a. YulNumCmp a => YulCat eff (a, a) BOOL
yulNumLt = YulJmpB (yulNumCmp @a (True , False, False))
yulNumLe = YulJmpB (yulNumCmp @a (True , True , False))
yulNumGt = YulJmpB (yulNumCmp @a (False, False, True ))
yulNumGe = YulJmpB (yulNumCmp @a (False, True , True ))
yulNumEq = YulJmpB (yulNumCmp @a (False, True , False))
yulNumNe = YulJmpB (yulNumCmp @a (True , False, True ))

-- | Wrapper for built-in keccak256 yul function.
yulKeccak256 :: forall eff a. YulO1 a => YulCat eff a B32
yulKeccak256 = YulJmpB ("__keccak_c_" ++ abiTypeCompactName @a, error "TODO: keccak_c")

------------------------------------------------------------------------------------------------------------------------
-- YulCat Exceptions
------------------------------------------------------------------------------------------------------------------------

-- | Revert without any message.
yulRevert :: forall eff a b. YulO2 a b => YulCat eff a b
yulRevert = YulDis >.> YulJmpB ("__revert_c_" ++ abiTypeCompactName @b, error "revert(0, 0)")

------------------------------------------------------------------------------------------------------------------------
-- YulCat Control Flows
------------------------------------------------------------------------------------------------------------------------

-- ^ 'IfThenElse' instance for 'YulCat' objects.
instance YulO2 a r => IfThenElse (YulCat eff r BOOL) (YulCat eff r a) where
  ifThenElse c a b = YulFork c YulId >.> YulITE a b

-- | Type alias of 'PatternMatchable' p for 'YulCat' objects.
type PatternMatchableYulCat eff p a = PatternMatchable (YulCat eff (p a)) (p a) (p (YulCat eff (p a) a)) YulCatObj

-- | Type class for building referenceable values.
class Referenceable a where
  yulRefGet :: REF a -> YulCat eff B32 a
  yulRefPut :: REF a -> YulCat eff (B32, a) ()

------------------------------------------------------------------------------------------------------------------------
-- SimpleNP Helpers
------------------------------------------------------------------------------------------------------------------------

--
-- UncurryingNP instances
--

-- ^ Base case: @uncurryingNP (x) => NP '[] -> x@
instance forall x r eff.
         ( YulO2 x r
         ) => UncurryingNP (x) '[] x (YulCat eff r) (YulCat eff r) (YulCat eff r) (YulCat eff r) Many where
  uncurryingNP x _ = x

-- ^ Inductive case: @uncurryingNP (x -> ...xs -> b) => (x, uncurryingNP (... xs -> b)) => NP (x:xs) -> b@
instance forall x xs b g r eff.
         ( YulO4 x (NP xs) b r
         , UncurryNP'Fst g ~ xs
         , UncurryNP'Snd g ~ b
         , UncurryingNP g xs b (YulCat eff r) (YulCat eff r) (YulCat eff r) (YulCat eff r) Many
         ) => UncurryingNP (x -> g) (x:xs) b (YulCat eff r) (YulCat eff r) (YulCat eff r) (YulCat eff r) Many where
  uncurryingNP f xxs = uncurryingNP @g @xs @b @(YulCat eff r) @(YulCat eff r) @(YulCat eff r) @(YulCat eff r) @Many
                       (f x) xs
    where xxs' = xxs  >.> YulSplit
          x    = xxs' >.> YulExl
          xs   = xxs' >.> YulExr

--
-- CurryingNP instances
--

-- ^ Base case: @curryingNP (NP '[] -> b) => b@
instance forall b r eff.
         ( YulO2 b r
         , LiftFunction b (YulCat eff r) (YulCat eff r) Many ~ YulCat eff r b
         ) => CurryingNP '[] b (YulCat eff r) (YulCat eff r) (YulCat eff r) Many where
  curryingNP cb = cb (YulDis >.> YulReduceType)

-- ^ Inductive case: @curryingNP (NP (x:xs) -> b) => x -> curryingNP (NP xs -> b)@
instance forall x xs b r eff.
         ( YulO5 x (NP xs) b (NP (x:xs)) r
         , CurryingNP xs b (YulCat eff r) (YulCat eff r) (YulCat eff r) Many
         ) => CurryingNP (x:xs) b (YulCat eff r) (YulCat eff r) (YulCat eff r) Many where
  curryingNP cb x = curryingNP @xs @b @(YulCat eff r) @(YulCat eff r)
                    (\xs -> cb (YulFork x xs >.> YulCoerceType))

------------------------------------------------------------------------------------------------------------------------
-- Stringify Functions
------------------------------------------------------------------------------------------------------------------------

-- | Compact and unique representation of 'YulCat', which can be used for generate its fingerprint.
--
--   Note:
--   * It is done so for the compactness of the string representation of the 'YulCat'.
yulCatCompactShow :: YulCat eff a b -> String
yulCatCompactShow = go
  where
    go :: YulCat eff' a' b' -> String
    go (YulReduceType @_ @a @b)    = "Tr" <> abi_type_name2 (Proxy :: Proxy (a, b))
    go (YulExtendType @_ @a @b)    = "Te" <> abi_type_name2 (Proxy :: Proxy (a, b))
    go (YulCoerceType @_ @a @b)    = "Tc" <> abi_type_name2 (Proxy :: Proxy (a, b))
    go (YulSplit @_ @xs)           = "Ts" <> abi_type_name (Proxy :: Proxy (NP xs))
    --
    go (YulId @_ @a)               = "id" <> abi_type_name (Proxy :: Proxy a)
    go (YulComp cb ac)             = "(" <> go ac <> ");(" <> go cb <> ")"
    go (YulProd ab cd)             = "(" <> go ab <> ")×(" <> go cd <> ")"
    go (YulSwap @_ @a @b)          = "σ" <> abi_type_name2 (Proxy :: Proxy (a, b))
    go (YulFork ab ac)             = "(" <> go ab <> ")▵(" <> go ac <> ")"
    go (YulExl @_ @a @b)           = "π₁" <> abi_type_name2 (Proxy :: Proxy (a, b))
    go (YulExr @_ @a @b)           = "π₂" <> abi_type_name2 (Proxy :: Proxy (a, b))
    go (YulDis @_ @a)              = "ε" <> abi_type_name (Proxy :: Proxy a)
    go (YulDup @_ @a)              = "δ" <> abi_type_name (Proxy :: Proxy a)
    --
    go (YulEmb @_ @a @b x)         = "{" <> escape x <> "}" <> abi_type_name2 (Proxy :: Proxy (a, b))
    go (YulJmpU @_ @a @b (fid, _)) = "Ju " <> fid <> abi_type_name2 (Proxy :: Proxy (a, b))
    go (YulJmpB @_ @a @b (fid, _)) = "Jb " <> fid <> abi_type_name2 (Proxy :: Proxy (a, b))
    go (YulITE a b)                = "?" <> "(" <> go a <> "):(" <> go b <> ")"
    --
    go (YulSGet @_ @a)             = "Sg" <> abi_type_name (Proxy :: Proxy a)
    go (YulSPut @_ @a)             = "Sp" <> abi_type_name (Proxy :: Proxy a)
    -- A 'abi_type_name variant, enclosing name with "@()".
    abi_type_name :: forall a. ABITypeable a => Proxy a -> String
    abi_type_name _ = "@" ++ abiTypeCompactName @a
    abi_type_name2 :: forall a b. (ABITypeable a, ABITypeable b) => Proxy (a, b) -> String
    abi_type_name2 _ = abi_type_name (Proxy :: Proxy a) ++ abi_type_name (Proxy :: Proxy b)
    -- TODO escape the value of x
    escape x = show x

-- | Obtain the finger print of a 'YulCat'.
yulCatFingerprint :: YulCat eff a b -> String
yulCatFingerprint = printf "%x" . digest_c8 . B.pack . show
  where c8 (_ :: Int) [] = 0
        c8 n (x:xs)      = (2 ^ n) * toInteger (ord x) + c8 (n + 8) xs
        digest_c8 bs = go_digest_c8 (B.splitAt 8 bs)
        go_digest_c8 (b, bs') = c8 0 (B.unpack b) `xorInteger`
                                if B.length bs' == 0 then 0 else digest_c8 bs'

instance Show (YulCat eff a b) where show = yulCatCompactShow
deriving instance Show AnyYulCat
