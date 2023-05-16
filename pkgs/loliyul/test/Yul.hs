{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LinearTypes        #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE UnicodeSyntax      #-}

import           Data.Coerce                  (Coercible, coerce)
import           Data.Constraint
import           Data.Proxy
import qualified Data.Text                    as T
import           GHC.Exts                     (TYPE)

import           Control.Category.Constrained hiding ((.))
import           Control.Category.Linear


-- FIXME, can't find a polymorphic one that replaces the ($)...
{-# INLINE ($$) #-}
($$) :: forall a b m. (a %m -> b) %m -> a %m -> b
f $$ x = f x
infixr 0 $$

{-# INLINE (&) #-}
(&) :: forall a b m. a %m -> (a %m -> b) %m -> b
x & f = f x

--------------------------------------------------------------------------------
-- YulCon Type Class
--------------------------------------------------------------------------------

-- | Instances of YulCon are objects in YulCat
class Show a => YulCon a where
  subYulCon :: forall c b. (YulCon (c ⊗ b), a ~ (c ⊗ b)) => Dict (YulCon c, YulCon b)
  subYulCon = error "YulCon::subYulCon only defined for objprod"

instance YulCon ()
instance (YulCon a, YulCon b) => YulCon (a⊗b) where
  subYulCon = Dict

instance ProdObj YulCon where
  prodobj = Dict
  objprod = subYulCon
  objunit = Dict

--------------------------------------------------------------------------------
-- Yul Types
--------------------------------------------------------------------------------

class YulCon a => YulVal a
class YulVal a => YulNum a

newtype YulAddr = YulAddr Integer deriving anyclass (YulCon, YulVal)

newtype YulBool = YulBool Bool deriving anyclass (YulCon, YulVal)
yBool :: Bool -> YulType
yBool = MkYulBool . YulBool

newtype YulUInt = YulUInt Integer deriving anyclass (YulCon, YulVal, YulNum)
yUInt :: Integer -> YulType
yUInt = MkYulUInt . YulUInt

newtype YulInt  = YulInt Integer deriving anyclass (YulCon, YulVal, YulNum)
yInt :: Integer -> YulType
yInt = MkYulInt . YulInt

data YulTuple = YulTupleNil | YulTupleCons YulType deriving YulCon

data YulType  = MkYulAddr YulAddr
              | MkYulTuple YulTuple
              | MkYulBool YulBool
              | MkYulUInt YulUInt
              | MkYulInt  YulInt
              deriving YulCon

deriving instance YulCon a => YulCon (Maybe a)

deriving instance Show YulAddr
deriving instance Show YulBool
deriving instance Show YulUInt
deriving instance Show YulInt
deriving instance Show YulTuple
deriving instance Show YulType

--------------------------------------------------------------------------------
-- The YulCat, a Symmetric Monoidal Category for Yul
--------------------------------------------------------------------------------

data YulCat a b where
  -- SMC Primitives
  YulIdentity :: YulCat a a
  YulCompose  :: YulCat c b -> YulCat a c -> YulCat a b
  YulProduct  :: YulCat c d -> YulCat p q -> YulCat (c⊗p) (d⊗q)
  YulUnitorL  :: YulCat a (a⊗())
  YulUnitorR  :: YulCat (a⊗()) a
  YulAssocL   :: YulCat ((a⊗b)⊗c) (a⊗(b⊗c))
  YulAssocR   :: YulCat (a⊗(b⊗c)) ((a⊗b)⊗c)
  YulSwap     :: YulCat (a⊗b) (b⊗a)
  YulDis      :: YulCat a ()
  YulDup      :: YulCat a (a⊗a)
  -- Yul Primitives
  YulConst    :: YulCon b => b -> YulCat a b
  YulNumPlus  :: YulNum a => YulCat (a⊗a) a
  YulSGet     :: YulVal a => YulCat YulAddr a
  YulSPut     :: YulVal a => YulCat (YulAddr⊗a) ()
  YulDetuple  :: YulVal a => YulCat YulTuple (Maybe a⊗YulTuple)
  YulJust     :: YulCat (Maybe a) a
  YulBlock    :: YulCat a b -> YCom b
  YulFunc     :: T.Text -> YulCat a b -> YFunction
  YulIntCb    :: YulTypeSpec -> YulCat () YulTuple
  -- YulExtCb    :: YulTypeSpec -> YulCat () YulTuple

data YulTypeSpec = YulTypeSpec deriving Show
type YCom a = YulCat () a
type YFunction = YulCat YulTuple YulTuple

instance Category YulCat where
  type Obj YulCat = YulCon
  id = YulIdentity
  (∘) = YulCompose

instance Monoidal YulCat where
  (×)     = YulProduct
  unitor  = YulUnitorL
  unitor' = YulUnitorR
  assoc   = YulAssocL
  assoc'  = YulAssocR
  swap    = YulSwap

instance Cartesian YulCat where
  dis = YulDis
  dup = YulDup

instance YulNum a => Num (YCom a) where

deriving instance Show (YulCat a b)

--------------------------------------------------------------------------------
-- The YulPort, a set of Linear Function APIs for YulCat
--------------------------------------------------------------------------------

type YulPort r a = P YulCat r a

instance YulNum a => Num (YulPort r a) where

yulNull :: forall r. YulCon r
        => YulPort r () ⊸ YulPort r ()
yulNull = encode $ YulIdentity

yulCom :: forall a. YulCon a
       => (forall r. YulCon r => YulPort r a) -> YCom a -- (YulPort r () -> YulPort r a)
yulCom c = decode (\x -> ignore x c)

yulConst :: forall r a b. (YulCon r, YulCon a, YulCon b)
         => b -> (YulPort r a ⊸ YulPort r b)
yulConst b = encode (YulConst b)

fst1 :: forall r a b. (YulCon r, YulCon a, YulCon b)
           => (YulPort r a ⊗ YulPort r b) ⊸ YulPort r a
fst1 (a, b) = encode YulUnitorR (merge (a, discard b))

yulSelect :: forall r a. (YulCon r, YulVal a)
             => Proxy a -> YulPort r YulTuple ⊸ YulPort r a
yulSelect _ a = encode YulJust (fst1 (split (encode YulDetuple a)))

yulPut :: forall r v. (YulCon r, YulVal v)
       => YulPort r YulAddr ⊸ YulPort r v ⊸ YulPort r ()
yulPut toP valP = encode (YulSPut) (merge (toP, valP))

yulPutTo :: forall r v. (YulCon r, YulVal v)
         => YulAddr -> YulPort r v ⊸ YulPort r ()
yulPutTo to valP = yulPut (yulConst to unit) valP

yulDefun :: T.Text
         -> (forall r. YulCon r => YulPort r YulTuple ⊸ YulPort r YulTuple)
         -> YFunction
yulDefun name f = YulFunc name (decode f)

yulDefun' :: T.Text -> YulTypeSpec
          -> (YCom YulTuple -> YCom YulTuple)
          -> YFunction
yulDefun' name spec f = YulFunc name (f (YulIntCb spec))
  -- where g' :: forall r2. YulCon r2 => YulPort r2 YulTuple ⊸ YulPort r2 YulTuple
  --       g' = encode (\p -> f (decode (\x -> ignore x p)))

--------------------------------------------------------------------------------
-- Contract Examples
--------------------------------------------------------------------------------

-- | A function that takes a UInt input and store its value doubled at a fixed storage location.
foo :: YFunction
foo = yulDefun "foo" $ \p ->
  let f :: YulCon r => YulPort r YulUInt ⊸ YulPort r YulUInt
      -- f v = v + dup v
      f v = (encode YulNumPlus) (copy v)
  in yulPutTo (YulAddr 0xdeadbeef) (f (yulSelect (Proxy @YulUInt) p)) &
     yulConst (YulTupleCons (yBool True))

foo' = yulDefun' "foo2" YulTypeSpec $ \x -> YulConst (YulTupleCons (yBool True))

--  \(YulDetuple (Just (a), _)) -> a + a

main = do
  putStrLn $ "yulNull:\n" <> show (decode $ yulNull :: YulCat () ()) <> "\n"
  putStrLn $ "yulConst:\n" <> show (decode $ yulConst (yInt 42) :: YulCat () YulType) <> "\n"
  putStrLn $ "foo:\n" <> show foo <> "\n"
  putStrLn $ "foo':\n" <> show foo' <> "\n"
