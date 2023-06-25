{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-overlapping-patterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Category.FreeSMC where

import Prelude hiding ((.),id,curry)
import Control.Category.Constrained
import Data.Monoid
import Data.Kind
import Data.Type.Equality

newtype Sho a b = Sho {fromSho :: Int -> ShowS}

instance Show (Sho a b) where
  showsPrec d (Sho f) = f d

shoCon :: String -> Sho a b
shoCon name = Sho $ \_ -> showString name

instance Category Sho where
  type Obj Sho = Trivial
  id = shoCon "id"
  Sho f ∘ Sho g = Sho $ \d -> showParen (d /= 0) (f 0 . showString " ∘ " . g 0)

instance Monoidal Sho where
  swap = shoCon "swap"
  assoc = shoCon "assoc"
  assoc' = shoCon "assoc'"
  unitor = shoCon "unitor"
  unitor' = shoCon "unitor'"
  Sho f × Sho g = Sho $ \d -> showParen (d /= 0) (f 2 . showString " × " . g 2)

instance Cartesian Sho where
  dis = shoCon "dis"
  dup = shoCon "dup"
  exl = shoCon "exl"
  exr = shoCon "exr"
  Sho f ▵ Sho g = Sho $ \d -> showParen (d /= 0) (f 2 . showString " ▵ " . g 2)

class HasShow k where
  toShow :: k a b -> Sho a b

instance HasShow Sho where
  toShow = id


instance (forall x y. (con x, con y) => Show (k x y)) => Show  (Cat k con a b) where
  show x = showsPrec (-1) x ""
  showsPrec d = \case
    I -> showString "id"
    S -> showString "swap"
    A  -> showString "assoc"
    A' -> showString "assoc'"
    U a -> {-showString "[" .-} fromSho (evalUnitor (trivializeUnitor a)) 0 {-. showString "]"-}
    U' a -> {-showString "[" .-} fromSho (evalUnitor' (trivializeUnitor a)) 0 {-. showString "]"-}
    X s -> showString (show s)
    f :.: g -> showParen (d >  0) (showsPrec 0 f . showString " ∘ " . showsPrec 0 g)
    f :×: g -> showParen (d > -1) (showsPrec 2 f . showString " × " . showsPrec 2 g)


showDbg :: Int -> Cat k con a b -> ShowS
showDbg d = \case
    X _ -> showString "?"

    I -> showString "id"
    f :.: g -> showParen (d /= 0) (showDbg 0 f . showString " ∘ " . showDbg 0 g)

    f :×: g -> showParen True (showDbg 2 f . showString " × " . showDbg 2 g)
    S -> showString "σ"
    A  -> showString "α"
    A' -> showString "α'"
    U _ -> showString "ρ"
    U' a  -> showString ("ρ'(" ++ show a ++ ")")



parens :: [Char] -> [Char]
parens x = "("<> x <>")"

mapGenerators :: (con a, con b) => (forall x y. (con x, con y) => k x y -> k' x y) -> Cat k con a b -> Cat k' con a b
mapGenerators f = \case
  X g -> X (f g)

  I -> I
  a :.: b -> mapGenerators f a :.: mapGenerators f b

  a :×: b -> mapGenerators f a :×: mapGenerators f b
  A -> A
  A' -> A'
  S -> S
  U x -> U x
  U' x -> U' x

  x -> error (showDbg 0 x " (Free.mapGenerators)")


instance Show (Unitor con a b) where
  show UL = "⟨"
  show UR = "⟩"
  show (IL a) = "⟨" ++ show a
  show (IR a) = "⟩" ++ show a
data Unitor con a b where
  UL :: Unitor con a ((),a)
  UR :: Unitor con a (a,())
  IL :: (con a, con b, con c) => Unitor con a b -> Unitor con (a,c) (b,c)
  IR :: (con a, con b, con c) => Unitor con a b -> Unitor con (c,a) (c,b)

compareUnitors :: Unitor con a b -> Unitor con a b' -> Maybe (b :~: b')
compareUnitors UL UL = Just Refl
compareUnitors UR UR = Just Refl
compareUnitors (IL a) (IL b) = case compareUnitors a b of Nothing -> Nothing; Just Refl -> Just Refl
compareUnitors (IR a) (IR b) = case compareUnitors a b of Nothing -> Nothing; Just Refl -> Just Refl
compareUnitors _ _ = Nothing

trivializeUnitor :: Unitor con a b -> Unitor Trivial a b
trivializeUnitor UL = UL
trivializeUnitor UR = UR
trivializeUnitor (IL f) = IL (trivializeUnitor f)
trivializeUnitor (IR f) = IR (trivializeUnitor f)

commuteUnitors :: (ProdObj con, forall α β. (con α, con β) => con (α,β), con (),
                  con a, con b) => Unitor con c b -> Unitor con a b -> Cat cat con a c
commuteUnitors UL UL = id
commuteUnitors UL UR = id
commuteUnitors UR UR = id
commuteUnitors UR UL = id
commuteUnitors (IL a) (IL b) = commuteUnitors a b × id
commuteUnitors (IR a) (IR b) = id × commuteUnitors a b
commuteUnitors (IR a) (IL b) = U (IL b) .  U' (IR a)
commuteUnitors (IL a) (IR b) = U (IR b) .  U' (IL a)
commuteUnitors UL (IR a) = U a . U' UL
commuteUnitors UR (IL a) = U a . U' UR
commuteUnitors (IR a) UL = U UL . U' a
commuteUnitors (IL a) UR = U UR . U' a


data Cat k (con :: Type -> Constraint) a b where
  A :: (con a, con b, con c) => Cat k con ((a,b),c) (a,(b,c))
  A' :: (con a, con b, con c) => Cat k con (a,(b,c)) ((a,b),c)
  S ::  (con a, con b) => Cat k con (a,b) (b,a) 
  Embed :: (con a, con b) => k a b -> Cat k con a b
  I :: Cat k con a a
  U :: Unitor con a b -> Cat k con a b
  U' :: Unitor con b a -> Cat k con a b

  (:.:) :: con b => (Cat k con b c) -> (Cat k con a b) -> (Cat k con a c)
  (:×:) :: (con a, con b, con c, con d) => (Cat k con a b) -> (Cat k con c d) -> (Cat k con (a ⊗ c) (b ⊗ d))


instance Invertible (Cat k con) where
  dual :: Cat k con a b -> Cat k con b a
  dual = \case
    I -> I
    f :×: g -> dual f :×: dual g
    f :.: g -> dual g :.: dual f
    S -> S
    A -> A'
    A' -> A

assocRight :: Cat k obj x y -> Cat k obj x y
assocRight (a :.: (assocRight -> (b :.: c))) = (a :.: b) :.: c
assocRight x = x

rightView :: (obj a, obj c) => Cat k obj a c -> Cat k obj a c
rightView (assocRight -> (a :.: b)) = a :.: b
rightView x = I :.: x

assocLeft :: Cat k obj x y -> Cat k obj x y
assocLeft ((assocLeft -> (a :.: b)) :.: c) = a :.: (b :.: c)
assocLeft x = x

leftView :: (obj a, obj c) => Cat k obj a c -> Cat k obj a c
leftView (assocLeft -> (a :.: b)) = a :.: b
leftView x = x :.: I

pattern (:>:) ::  (obj x, obj y) => (obj b)  =>  Cat k obj b y -> Cat k obj x b -> Cat k obj x y
pattern f :>: g <- (rightView -> f :.: g)
  where f :>: g = f . g

pattern (:<:) ::  (obj x, obj y) => (obj b) => Cat k obj b y -> Cat k obj x b -> Cat k obj x y
pattern f :<: g <- (leftView -> f :.: g)
  where f :<: g = f . g

-- pattern Uncurry :: (obj a1, obj a2, obj c, obj (a1×a2)) => Cat k obj a1  (a2 -> c) -> Cat k obj (a1 × a2)  c
-- pattern Uncurry f <- Apply :<: (f :×: I)



evalM :: forall k a b con.
              (ProdObj con, forall x y. (con x, con y) => con (x,y), con (),
               con ~ Obj k, Monoidal k, Obj k a, Obj k b) => Cat k (Obj k) a b -> k a b
evalM I          = id
evalM (f :×: g)  = evalM f × evalM g
evalM (f :.: g)  = evalM f . evalM g
evalM A          = assoc
evalM A'         = assoc'
evalM S          = swap
evalM (U u)      = evalUnitor u
evalM (U' u)     = evalUnitor' u
evalM (Embed ϕ)  = ϕ

evalCartesian :: forall k a b con.
              (ProdObj con, forall x y. (con x, con y) => con (x,y), con (),
               con ~ Obj k, Cartesian k, Obj k a, Obj k b) => Cat k (Obj k) a b -> k a b
evalCartesian = \case
  I -> id
  (f :×: g) -> evalCartesian f × evalCartesian g
  (f :.: g) -> evalCartesian f . evalCartesian g
  (X ϕ) -> ϕ
  A -> assoc
  A' -> assoc'
  S -> swap
  (U u) -> evalUnitor u
  (U' u) -> evalUnitor' u


evalUnitor :: forall k a b con.
              (ProdObj con, forall x y. (con x, con y) => con (x,y), con (),
               con ~ Obj k, Monoidal k, Obj k a, Obj k b)
           => Unitor (Obj k) a b -> k a b
evalUnitor UR = unitor
evalUnitor UL = swap . unitor
evalUnitor (IL x) = evalUnitor x × id
evalUnitor (IR x) = id × evalUnitor x

evalUnitor' :: forall k a b con.
              (ProdObj con, forall x y. (con x, con y) => con (x,y), con (),
               con ~ Obj k, Monoidal k, Obj k a, Obj k b)
           => Unitor (Obj k) b a -> k a b
evalUnitor' UR = unitor'
evalUnitor' UL = unitor' . swap
evalUnitor' (IL x) = evalUnitor' x × id
evalUnitor' (IR x) = id × evalUnitor' x
-- eval Dup = dup
-- eval Apply = apply
-- eval (Curry f) = curry (eval f)
---------------------------
-- Cat k obj - instances


pattern X :: forall (k :: Type -> Type -> Type) (con :: Type -> Constraint) a b. () => (con a, con b) => k a b -> Cat k con a b
pattern X x = Embed x

instance Category (Cat k con) where
  type Obj (Cat k con) = con
  id = I
  I ∘ x = x
  x ∘ I = x
  x ∘ y = x :.: y

instance (ProdObj con, forall a b. (con a, con b) => con (a,b)) =>  Monoidal (Cat k con) where
  I × I = I
  U' a × I = U' (IL a)
  I × U' a = U' (IR a)
  f × g = f :×: g
  assoc =  A
  assoc' = A'
  swap = S
  unitor = U UR
  unitor' = U' UR


type Composer k con = forall a b c. (con a, con b, con c) => Cat k con b c -> Cat k con a b  -> Cat k con a c
type PartialComposer k con = forall a b c. (con a, con b, con c) => Cat k con b c -> Cat k con a b  -> Alt Maybe (Cat k con a c)
type ProtoSimplifier k con = (con (), ProdObj con, forall a b. (con a, con b) => con (a,b)) => Composer k con -> PartialComposer k con
type Simplifier k con = (con (), ProdObj con, forall a b. (con a, con b) => con (a,b)) => forall a b. (con a, con b) =>  Cat k con a b -> Cat k con a b

monoidalSimplify :: (con (), ProdObj con, forall α β. (con α, con β) => con (α,β)) => (con a, con b) => Cat k con a b -> Cat k con a b
monoidalSimplify = mkSimplifier monoidalRules

monoidalRules :: forall k con. ProtoSimplifier k con
monoidalRules  (.) = \ x y -> Alt (after x y) where
  after :: (con a, con b, con c) => Cat k con b c -> Cat k con a b -> Maybe (Cat k con a c)

  -- obvious simplifications
  S `after` S = Just id
  A' `after` A = Just id
  A `after` A' = Just id

  -- commute (or cancel) unitors
  U' x `after` U y = Just (commuteUnitors x y)

  -- push swaps to the right
  S `after` (f :×: g) = Just ((g × f) . S)

  -- swap individual strands
  S `after` A = Just (assoc' . (id × swap) . assoc . (swap × id))
  S `after` A' = Just (assoc . (swap × id) . assoc' . (id × swap))
  A  `after` S = Just ((id × swap) . assoc  . (swap × id) . assoc')
  A' `after` S = Just ((swap × id) . assoc'  . (id × swap ) . assoc)

  -- push U' through S
  U' UR `after` S = Just (U' UL)
  U' UL `after` S = Just (U' UR)
  U' (IL a) `after` S = Just (swap . U' (IR a))
  U' (IR a) `after` S = Just (swap . U' (IL a))

  -- push U' into ×
  U' UR `after` ((f :×: I) :<: h) = Just (f . U' UR  . h)
  U' (IL a) `after` ((f :×: g) :<: h) = Just (((U' a . f) × g)  . h )
  U' (IR a) `after` ((f :×: g) :<: h) = Just ((f × (U' a . g))  . h )

  -- push U' through A'
  U' UR `after` A' = Just (id × U' UR)
  U' (IR a) `after` A' = Just (A' . (id × (id × U' a)))
  U' (IL (IR a)) `after` A' = Just (A' . (id × (U' a × id)) )
  U' (IL UR) `after` A' = Just (id × U' UL )
  U' (IL UL)     `after` A' = Just (U' UL)
  U' (IL (IL a)) `after` A' = Just (A' . (U' a × id))

  -- push U' through A
  U' UL          `after` A = Just (U' UL × id)
  U' (IL a)      `after` A = Just (A . ((U' a × id) × id))
  U' (IR (IL a)) `after` A = Just (A . ((id × U' a) × id))
  U' (IR UL)     `after` A = Just (U' UR × id)
  U' (IR UR)     `after` A = Just (U' UR)
  U' (IR (IR a)) `after` A = Just (A . (id × U' a))

  -- compose strands 
  (f :×: g) `after` (h :×: i) = Just ((f . h) × (g . i))


  -- failing the above, extract unitors
  ((f :>: U' a) :×: g) `after` h = Just ((f×g) . U' (IL a) . h )
  (f :×: (g :>: U' a)) `after` h = Just ((f × g) . U' (IR a) . h)

  h `after` ((f :>: U' a) :×: g) = Just (h . (f × g) . U' (IL a) )
  h `after` (f :×: (g :>: U' a)) = Just (h . (f × g) . U' (IR a) )


  -- extract unitors from ▵:
  -- h `after` ((U a :<: f) :▵: g) = Just (h . U (IL a) . (f ▵ g)  )
  -- h `after` (f :▵: (U a :<: g )) = Just (h . U (IR a) . (f ▵ g) )

  _ `after` _ = Nothing


neverEqual :: Comparator k
neverEqual _ _ = Nothing


mkSimplifier :: forall k con. ProtoSimplifier k con -> Simplifier k con
mkSimplifier protoAfter = simplify where
   (...) :: Composer k con
   I ... g  = g -- g is already normal.
   f ... I  = f -- f is already normal.
   (f :>: g) ... (h :<: i) = case getAlt (g `after` h) of
     Nothing -> (f :>: g) :.: (h :<: i) -- no reaction.  both subterms are normal. so we're done.
     Just j -> f ... j ... i --- reaction :: we must recurse. ("After" must return a normal term; j.)
   f ... g = f :.: g
   after :: PartialComposer k con
   after = protoAfter (...)

   simplify :: (con a, con b) => Cat k con a b -> Cat k con a b
   -- simplify (Curry f) = Curry (simplify f)
   simplify (f :×: g) = simplify f × simplify g
   simplify (f :.: g) = simplify f ... simplify g
   simplify x = x


toDup :: (ProdObj con, forall x y. (con x, con y) => con (x,y), con (), con a, con b) => Cat k con a b -> Cat k con a b
toDup = \case
  I -> I
  (f :×: g) -> toDup f × toDup g
  (f :.: g) -> toDup f . toDup g
  X ϕ -> X ϕ
  A -> A
  A' -> A'
  S -> S
  (U u) -> U u
  (U' u) -> U' u


toE :: (ProdObj con, forall x y. (con x, con y) => con (x,y), con (), con a, con b) => Cat k con a b -> Cat k con a b
toE = \case
  I -> I
  (f :×: g) -> toE f × toE g
  (f :.: g) -> toE f . toE g
  X ϕ -> X ϕ
  A -> A
  A' -> A'
  S -> S
  (U u) -> U u
  (U' u) -> U' u

