{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LinearTypes         #-}
{-|
Copyright   : (c) 2023-2024 Miao, ZhiCheng
License     : LGPL-3
Maintainer  : hellwolf@yolc.dev
Stability   : experimental

= Description

[Yul](https://docs.soliditylang.org/en/latest/yul.html) is an intermediate language that is part of the [solidity
compiler](https://docs.soliditylang.org/en/latest/). It is by-design aspiring to be compiled to bytecode for
different backends, while at the moment it is for [Ethereum Virtual
Machine](https://ethereum.org/en/developers/docs/evm/) (EVM).

This module provides an "Embedded (in Haskell) Domain Specific Language" (eDSL) for programming in Yul, called
'YulCat'.

Further more, the 'YulCat' is instantiated as a "Symmetric Monoidal Category" (SMC). Being a SMC enables the possibility
for compiling linearly-typed functions in Haskell directly to the 'YulCat', that is believed to greatly enhance the
ergonomics of programming in 'YulCat'.

YulCat is designed to be a category. The objects in this category are instances of 'YulCatObj'.

-}
module YulDSL.Core.YulCat
  ( NonPureEffect, IsPureEffect, IsNonPureEffect
  , YulJmpTarget(UserDefinedYulCat, BuiltInYulJmpTarget)
  , YulCat (..), AnyYulCat (..)
  , (>.>), (<.<), emb, jmpUserDefined, jmpBuiltIn
  , digestYulCat
  , yulNumLt, yulNumLe, yulNumGt, yulNumGe, yulNumEq, yulNumNe
  , IfThenElse (ifThenElse)
  , PatternMatchable (inCase, match)
  , PatternMatchableYulCat
  ) where

-- base
import           Data.Char                (ord)
import           Data.Kind                (Constraint)
import           GHC.Integer              (xorInteger)
import           GHC.TypeError            (Assert, ErrorMessage (Text), TypeError)
import           Text.Printf              (printf)
-- bytestring
import qualified Data.ByteString.Char8    as B
-- eth-abi
import           Ethereum.ContractABI
--
import           Control.IfThenElse
import           Control.PatternMatchable
--
import           YulDSL.Core.YulCatObj
import           YulDSL.Core.YulNum


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

-- | Internal jump targets.
data YulJmpTarget eff a b
  = UserDefinedYulCat   (String, YulCat eff a b) -- ^ (Target id, an user-defined morphism)
  | BuiltInYulJmpTarget (String, a -> b)         -- ^ (Target id, a semantically-equivalent evaluation function)

-- | A GADT-style DSL of Yul that constructs morphisms between objects (YulCatObj) of the "Yul Category".
--
--  Note: - The inhabitants of this are actually morphisms of the Yul category. "Cat" is just a nice sounding moniker,
--  while the actual category is "Yul Category".
data YulCat (eff :: k) a b where
  -- * Type-level Operations (zero yul code)
  --
  -- ^ Convert from extended yul object to its core yul object.
  YulReduceType :: forall eff a b. (YulO2 a b, ABITypeDerivedOf a ~ b) => YulCat eff a b
  -- ^ Extend core yul object type.
  YulExtendType :: forall eff a b. (YulO2 a b, a ~ ABITypeDerivedOf b) => YulCat eff a b
  -- ^ Convert between coercible yul objects.
  YulCoerceType :: forall eff a b. (YulO2 a b, ABITypeCoercible a b) => YulCat eff a b
  -- ^ Split the head and tail of a n-ary product where n >= 1.
  YulSplit :: forall eff as. YulO1 (NP as) => YulCat eff (NP as) (Head as, NP (Tail as))

  -- * SMC Primitives
  --
  -- ** Category
  YulId   :: forall eff a.     YulO2 a a   => YulCat eff a a
  YulComp :: forall eff a b c. YulO3 a b c => YulCat eff c b %1-> YulCat eff a c %1-> YulCat eff a b
  -- ** Monoidal
  YulProd :: forall eff a b c d. YulO4 a b c d => YulCat eff a b %1-> YulCat eff c d %1-> YulCat eff (a, c) (b, d)
  YulSwap :: forall eff a b.     YulO2 a b     => YulCat eff (a, b) (b, a)
  -- ** Cartesian
  YulFork :: forall eff a b c. YulO3 a b c => YulCat eff a b %1-> YulCat eff a c %1-> YulCat eff a (b, c)
  YulExl  :: forall eff a b.   YulO2 a b   => YulCat eff (a, b) a
  YulExr  :: forall eff a b.   YulO2 a b   => YulCat eff (a, b) b
  YulDis  :: forall eff a.     YulO1 a     => YulCat eff a ()
  YulDup  :: forall eff a.     YulO1 a     => YulCat eff a (a, a)

  -- * Control Flow Primitives
  --
  -- ^ Embed a constant value @a@ and disregard any input object @b@.
  YulEmb :: forall eff a b. YulO2 a b => a %1-> YulCat eff b a
  -- ^ Jump an internal yul function by reference its identifier along with maybe a definition.
  YulJmp :: forall eff a b. YulO2 a b => YulJmpTarget eff a b %1-> YulCat eff a b
  -- ^ If-then-else expression.
  YulITE :: forall eff a. YulO1 a => YulCat eff (BOOL, (a, a)) a

  -- * Effectful Primitives
  --
  -- ^ Get storage word.
  YulSGet :: forall eff a. (IsNonPureEffect eff, YulO1 a, ABIWordValue a) => YulCat eff ADDR a
  -- ^ Put storage word.
  YulSPut :: forall eff a. (IsNonPureEffect eff, YulO1 a, ABIWordValue a) => YulCat eff (ADDR, a) ()

-- | Existential wrapper of the 'YulCat'.
data AnyYulCat = forall eff a b. YulO2 a b => MkAnyYulCat (YulCat eff a b)

------------------------------------------------------------------------------------------------------------------------
-- YulCat Verbial Utilities
------------------------------------------------------------------------------------------------------------------------

-- | Left to right composition of 'YulCat'.
(>.>) :: forall eff a b c. YulO3 a b c => YulCat eff a b %1-> YulCat eff b c %1-> YulCat eff a c
m >.> n = n `YulComp` m

-- | Right-to-left composition of 'YulCat'.
(<.<) :: forall eff a b c. YulO3 a b c => YulCat eff b c %1-> YulCat eff a b %1-> YulCat eff a c
(<.<) = YulComp

-- Same precedence as (>>>) (<<<);
-- see https://hackage.haskell.org/package/base-4.20.0.1/docs/Control-Category.html
infixr 1 >.>, <.<

-- | Embed a constant yul-categorical value.
emb :: forall eff a r. YulO2 r a => a -> YulCat eff r a
emb = YulEmb

-- | Short-hand for 'YulJmp' to user-defined 'YulCat'.
jmpUserDefined :: forall eff a b. YulO2 a b => (String, YulCat eff a b) %1-> YulCat eff a b
jmpUserDefined tgt = YulJmp (UserDefinedYulCat tgt)

-- | Short-hand for 'YulJmp' to built-in target.
jmpBuiltIn :: forall eff a b. YulO2 a b => BuiltInYulFunction a b %1-> YulCat eff a b
jmpBuiltIn tgt = YulJmp (BuiltInYulJmpTarget tgt)

-- YulNum Ord operations:

yulNumLt :: forall eff a. YulNumCmp a => YulCat eff (a, a) BOOL
yulNumLe :: forall eff a. YulNumCmp a => YulCat eff (a, a) BOOL
yulNumGt :: forall eff a. YulNumCmp a => YulCat eff (a, a) BOOL
yulNumGe :: forall eff a. YulNumCmp a => YulCat eff (a, a) BOOL
yulNumEq :: forall eff a. YulNumCmp a => YulCat eff (a, a) BOOL
yulNumNe :: forall eff a. YulNumCmp a => YulCat eff (a, a) BOOL

yulNumLt = jmpBuiltIn (yulNumCmp @a (True , False, False))
yulNumLe = jmpBuiltIn (yulNumCmp @a (True , True , False))
yulNumGt = jmpBuiltIn (yulNumCmp @a (False, False, True ))
yulNumGe = jmpBuiltIn (yulNumCmp @a (False, True , True ))
yulNumEq = jmpBuiltIn (yulNumCmp @a (False, True , False))
yulNumNe = jmpBuiltIn (yulNumCmp @a (True , False, True ))

-- ^ 'IfThenElse' instance for 'YulCat' objects.
instance YulO2 a r => IfThenElse (YulCat eff r BOOL) (YulCat eff r a) where
  ifThenElse c a b = YulITE <.< YulFork c (YulFork a b)

-- | Type alias of 'PatternMatchable' for 'YulCat' objects.
type PatternMatchableYulCat eff p a = PatternMatchable (YulCat eff (p a)) (p a) (p (YulCat eff (p a) a)) YulCatObj

------------------------------------------------------------------------------------------------------------------------
-- NP Helpers
------------------------------------------------------------------------------------------------------------------------

--
-- UncurryingNP instances
--

-- (x)
instance forall x r eff.
         ( YulO2 x r
         ) => UncurryingNP (x) '[] x (YulCat eff r) (YulCat eff r) (YulCat eff r) (YulCat eff r) Many where
  uncurryingNP x _ = x

-- (x -> ...xs -> b)
instance forall x xs b g r eff.
         ( YulO4 x (NP xs) b r
         , UncurryNP'Fst g ~ xs
         , UncurryNP'Snd g ~ b
         , UncurryingNP g xs b (YulCat eff r) (YulCat eff r) (YulCat eff r) (YulCat eff r) Many
         ) => UncurryingNP (x -> g) (x:xs) b (YulCat eff r) (YulCat eff r) (YulCat eff r) (YulCat eff r) Many where
  uncurryingNP f xxs = uncurryingNP @g @xs @b @(YulCat eff r) @(YulCat eff r) @(YulCat eff r) @(YulCat eff r) @Many
                       (f x) xs
    where xxs' = xxs >.> YulSplit
          x   = xxs' >.> YulExl
          xs  = xxs' >.> YulExr

--
-- CurryingNP instances
--

instance forall b r eff.
         ( YulO2 b r
         , LiftFunction b (YulCat eff r) (YulCat eff r) Many ~ YulCat eff r b
         ) => CurryingNP '[] b (YulCat eff r) (YulCat eff r) (YulCat eff r) Many where
  curryingNP cb = cb (YulDis >.> YulReduceType)

instance forall x xs b r eff.
         ( YulO5 x (NP xs) b (NP (x:xs)) r
         , CurryingNP xs b (YulCat eff r) (YulCat eff r) (YulCat eff r) Many
         ) => CurryingNP (x:xs) b (YulCat eff r) (YulCat eff r) (YulCat eff r) Many where
  curryingNP cb x = curryingNP @xs @b @(YulCat eff r) @(YulCat eff r)
                    (\xs -> cb (YulFork x xs >.> YulCoerceType))

------------------------------------------------------------------------------------------------------------------------
-- Show Instance For Unique String Representation Of Cats
------------------------------------------------------------------------------------------------------------------------

-- | Compact-and-unique-representational show instance for 'YulCat'.
--
--   Note:
--   * It is deliberately done so for compactness of the string representation of the 'YulCat'.
--   * It is meant also for strong equality checking of 'YulCat' used in yul object building.
instance Show (YulCat eff a b) where
  show (YulReduceType) = "Tred" <> abi_type_name @a <> abi_type_name @b
  show (YulExtendType) = "Text" <> abi_type_name @a <> abi_type_name @b
  show (YulCoerceType) = "Tcoe" <> abi_type_name @a <> abi_type_name @b
  show (YulSplit)      = "Tspl" <> abi_type_name @a
  --
  show (YulId)         = "id"
  show (YulComp cb ac) = "(" <> show ac <> ");(" <> show cb <> ")"
  show (YulProd ab cd) = "(" <> show ab <> ")×(" <> show cd <> ")"
  show (YulSwap)       = "σ" <> abi_type_name @a <> abi_type_name @b
  show (YulFork ab ac) = "(" <> show ab <> ")▵(" <> show ac <> ")"
  show (YulExl)        = "π₁" <> abi_type_name @a
  show (YulExr)        = "π₂" <> abi_type_name @a
  show (YulDis)        = "ε" <> abi_type_name @a
  show (YulDup)        = "δ" <> abi_type_name @a
  --
  show (YulEmb x)      = "{" <> show x <>  "}" -- escape x
  show (YulJmp tgt)    = case tgt of
    (UserDefinedYulCat (cid, _))   -> "Ju " <> cid
    (BuiltInYulJmpTarget (cid, _)) -> "Jb " <> cid
  show (YulITE)        = "?" <> abi_type_name @a
  --
  show (YulSGet)       = "Sget" <> abi_type_name @a
  show (YulSPut)       = "Sput" <> abi_type_name @a

instance Show AnyYulCat where
  show (MkAnyYulCat c) = show c

-- | It's not as scary as it sounds. It produces a fingerprint for the morphism.
digestYulCat :: YulCat eff a b -> String
digestYulCat = printf "%x" . digest_c8 . B.pack . show
  where c8 _ []     = 0
        c8 n [a]    = (2 ^ n) * toInteger(ord a)
        c8 n (a:as) =  (2 ^ n) * toInteger(ord a) + c8 (n + 8) as
        digest_c8 bs = go_digest_c8 (B.splitAt 8 bs)
        go_digest_c8 (b, bs') = c8 (0 :: Integer) (B.unpack b) `xorInteger`
                                if B.length bs' == 0 then 0 else digest_c8 bs'

------------------------------------------------------------------------------------------------------------------------
-- INTERNAL FUNCTIONs
------------------------------------------------------------------------------------------------------------------------

-- A 'abi_type_name variant, enclosing name with "@()".
abi_type_name :: forall a. ABITypeable a => String
abi_type_name = "@" ++ abiTypeCompactName @a
