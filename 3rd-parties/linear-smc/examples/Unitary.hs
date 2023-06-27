{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE LinearTypes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-overlapping-patterns #-}

import Control.Category.Constrained
import Control.Category.Linear

import Data.Complex
import Data.List (transpose, intercalate)
import Data.Constraint
import Control.Monad
import System.Exit (exitFailure)
import Prelude hiding (id,(.),not)
import Data.Array
import Numeric

type COMPLEX = Complex Double


data U a b = U {fromM :: Array (a,b) COMPLEX}

class (Bounded a, Ix a, Eq a) => Finite a where
  inhabitants :: [a]
  subFinite :: forall c b. (Finite (c ⊗ b), a ~ (c ⊗ b)) => Dict (Finite c, Finite b)
instance Finite () where
  inhabitants = [()]
  subFinite = error "Finite ()"
instance Finite Bool where
  inhabitants = inhabitants'
  subFinite = error "Finite Bool"

inhabitants' :: forall a. (Bounded a, Enum a) => [a]
inhabitants' = [minBound..maxBound]

pad :: Int -> String -> String
pad n xs = replicate (n - length xs) ' ' ++ xs

padAll :: [String] -> [String]
padAll xs = map (pad m) xs
  where m = maximum (map length xs)

showMat :: [[String]] -> String
showMat = unlines . map (intercalate "  ") . transpose . map padAll . transpose 

showCOMPLEX :: RealFloat a => Complex a -> String
showCOMPLEX (x :+ y) =  (showFFloat (Just 1) x  . showString "+i" . showFFloat (Just 1) y ) ""

instance (Finite a, Finite b) => Show (a `U` b) where
  show (U f) = showMat [[showCOMPLEX (f ! (i,j))  | i <- inhabitants] | j <- inhabitants]


instance (Finite a, Finite b) => Finite (a,b) where
  inhabitants = [(x,y) | x <- inhabitants, y <- inhabitants]
  subFinite = Dict

instance ProdObj Finite where
  prodobj = Dict
  objprod = subFinite
  objunit = Dict

summation :: (Num a, Finite t) => (t -> a) -> a
summation f = sum [f x | x <- inhabitants]

tabulate :: (Finite a, Finite b) => (a -> b -> COMPLEX) -> a `U` b
tabulate f = U (  array (  (minBound,minBound),
                           (maxBound,maxBound))
                    [((i,j),f i j) | i <- inhabitants, j <- inhabitants])

instance Category U where
  type Obj U = Finite
  id = tabulate delta   -- identity matrix
  U g ∘ U f = tabulate (\i j ->  summation
                                 (\k -> f!(i,k)  *  g!(k,j)))   -- matrix multiplication

instance Monoidal U where
  U f × U g = tabulate (\(a,c) (b,d) -> f ! (a,b) * g ! (c,d)) -- kroneckerProduct
  unitor = tabulate (\x (y,()) -> delta x y)
  unitor' = tabulate (\(y,()) x -> delta x y)
  assoc = tabulate  (\  ((x,y),z) (x',(y',z')) ->
                        delta ((x,y),z) ((x',y'),z'))
  assoc' = tabulate  (\  (x',(y',z')) ((x,y),z) ->
                         delta ((x,y),z) ((x',y'),z'))
  swap = tabulate $ \(x,y) (y',x') -> delta (x,y) (x',y')
 
-- This is indeed a tensor product.
-- Assume z with two untangled parts: z[(i,k)] = x[i] + y[k]
-- Consider: ((f×g) · z) (j,l)
-- = ∑i ∑k (f×g)(j,l)(i,k) z[i,k]
-- = ∑i ∑k f(j,i) * g(l,k) * (x[i] + y[k])
-- = ∑i ∑k f(j,i) * g(l,k) * x[i]    +   ∑i ∑k f(j,i) * g(l,k) * y[k]
-- = ∑i  f(j,i) * x[i] * ∑k g(l,k)   +   ∑k g(l,k) * y[k] * ∑i f(j,i)
-- = ∑i  f(j,i) * x[i] *    1        +   ∑k g(l,k) * y[k] *     1
-- =  (f · x)(j)                     +   (g · y)(l)


-- instance Cartesian (U) where
--   dup = tabulate $ \i (j,k) -> if i==j && i==k then one else 0
--   exl = tabulate $ \(i,_) k -> if i==k then 1 else 0
--   exr = tabulate $ \(_,i) k -> if i==k then 1 else 0

-- instance CoCartesian (U) where
--   jam = tabulate $ \(j,k) i -> if i==j && i==k then 1 else 0
--   inl = tabulate $ \k (i,_) -> if i==k then 1 else 0
--   inr = tabulate $ \k (_,i) -> if i==k then 1 else 0

-- instance Frobenius COMPLEX (U) where
--   scale c = tabulate $ \i j -> c * delta i j



vsq2 :: COMPLEX
vsq2 = 1 / sqrt 2

-- Hadamard (H) gate
h :: Finite r => P U r Bool ⊸ P U r Bool
h = encode (tabulate m) where
  m :: Bool -> Bool -> COMPLEX
  m True True = -vsq2
  m _ _       =  vsq2


decoded_h :: U Bool Bool
decoded_h = decode h

-- >>> decoded_h
-- 0.7071067811865475 :+ 0.0       0.7071067811865475 :+ 0.0
-- 0.7071067811865475 :+ 0.0 (-0.7071067811865475) :+ (-0.0)

i :: COMPLEX
i = 0 :+ 1

t :: Finite r => P (U) r Bool ⊸ P (U) r Bool
t = encode (tabulate m) where
  m :: Bool -> Bool -> COMPLEX
  m True True = exp (i*pi/4)
  m False False = 1
  m _ _       =  0

hermitianConjugate :: (Finite a, Finite b) => b `U` a -> a `U` b 
hermitianConjugate (U f) =  tabulate (\i j -> conjugate (f ! (j,i)))

conjugateTranspose :: {-<-}(Finite a, Finite b) =>{->-} U b a -> U a b
conjugateTranspose = hermitianConjugate 

-- Lets' not show the type; it works also for M matrices.

invert :: {-<-} (Finite a, Finite b) => {->-} (forall s. {-<-} Finite s => {->-} P U s a ⊸ P U s b) -> (forall r. {-<-} Finite r => {->-} P U r b ⊸ P U r a)
invert f = encode (conjugateTranspose (decode f))

t' :: Finite r => P U r Bool ⊸ P U r Bool
t' = invert t

t'decoded :: U Bool Bool
t'decoded = decode t'

-- >>> t'decoded
-- 1.0 :+ (-0.0)                                0.0 :+ (-0.0)
-- 0.0 :+ (-0.0)  0.7071067811865476 :+ (-0.7071067811865475)

ctrl :: Finite a =>  (forall r. Finite r => P (U) r a ⊸ P (U) r a) ->
                     (forall r. Finite r => P (U) r Bool ⊸ P (U) r a ⊸ P (U) r (Bool,a))
ctrl f x y = encode (ctrlMat (decode f)) (x !: y)

ctrlMat :: Finite a => U a a -> U (Bool,a) (Bool,a)
ctrlMat (U f) = tabulate (\(cIn,x) (cOut,y) ->
        case (cIn,cOut) of
          (True,True) -> f!(x,y) -- if the control is active, transform using f
          (False,False) -> delta x y -- otherwise identity 
          _ -> 0) -- never transform the control


delta :: (Eq a) => a -> a -> COMPLEX
delta x y = if x == y then 1 else 0


not :: Finite r => P (U) r Bool ⊸ P (U) r Bool
not = encode ((tabulate $ \x y -> 1 - delta x y))

(&) ::  a ⊸ (a ⊸ b) ⊸ b
x & f = f x

ctrlneg' :: U (Bool,Bool) (Bool,Bool)
ctrlneg' = decode (\p -> split p & \(x,y) -> ctrl not x y)

-- >>> ctrlneg'
-- 1.0 :+ 0.0  0.0 :+ 0.0  0.0 :+ 0.0  0.0 :+ 0.0
-- 0.0 :+ 0.0  1.0 :+ 0.0  0.0 :+ 0.0  0.0 :+ 0.0
-- 0.0 :+ 0.0  0.0 :+ 0.0  0.0 :+ 0.0  1.0 :+ 0.0
-- 0.0 :+ 0.0  0.0 :+ 0.0  1.0 :+ 0.0  0.0 :+ 0.0




second :: (t ⊸ b) ⊸ (a, t) ⊸ (a, b)
second f (x,y) = (x,f y)

first :: (t ⊸ b) ⊸ (t, a) ⊸ (b, a)
first f (x,y) = (f x,y)


toffoli2 :: (Obj k r, Obj k (b, b), Obj k b, Monoidal k, con (), (forall α β. (con α, con β) => con (α,β)), con ~ Obj k) =>
                (P k r b ⊸ P k r b)
                -> (P k r b ⊸ P k r b)
                -> (P k r b ⊸ P k r b)
                -> (P k r b ⊸ P k r b ⊸ (P k r (b,b)))
                -> ((P k r b, P k r b), P k r b)
                ⊸ P k r ((b, b), b)

toffoli2 {-<-} hadam tGate tInv cnot {->-}  ((c1,c2),x) =
   cnot c1          (hadam    x)   & split & \(c1,x)  ->
   cnot c2          (tInv     x)   & split & \(c2,x)  ->
   cnot c1          (tGate    x)   & split & \(c1,x)  ->
   cnot c2          (tInv     x)   & split & \(c2,x)  ->
   cnot c2          (tGate    c1)  & split & \(c2,y)  ->
   (cnot (tGate c2) (tInv y)) !: (hadam (tGate x))
  
toffU :: Finite r => P U r ((Bool, Bool), Bool) ⊸ P (U) r ((Bool, Bool), Bool)
toffU = toffoli2 h t t' (ctrl not) . first split . split


toffoli'' :: U ((Bool, Bool), Bool) ((Bool, Bool), Bool)
toffoli'' = decode toffU

result :: String
result = "1.0+i0.0  0.0+i0.0  0.0+i0.0  0.0+i0.0  0.0+i0.0  0.0+i0.0   0.0+i0.0   0.0+i0.0\n\
         \0.0+i0.0  1.0+i0.0  0.0+i0.0  0.0+i0.0  0.0+i0.0  0.0+i0.0   0.0+i0.0   0.0+i0.0\n\
         \0.0+i0.0  0.0+i0.0  0.0+i0.0  0.0+i0.0  1.0+i0.0  0.0+i0.0   0.0+i0.0   0.0+i0.0\n\
         \0.0+i0.0  0.0+i0.0  0.0+i0.0  0.0+i0.0  0.0+i0.0  1.0+i0.0   0.0+i0.0   0.0+i0.0\n\
         \0.0+i0.0  0.0+i0.0  1.0+i0.0  0.0+i0.0  0.0+i0.0  0.0+i0.0   0.0+i0.0   0.0+i0.0\n\
         \0.0+i0.0  0.0+i0.0  0.0+i0.0  1.0+i0.0  0.0+i0.0  0.0+i0.0   0.0+i0.0   0.0+i0.0\n\
         \0.0+i0.0  0.0+i0.0  0.0+i0.0  0.0+i0.0  0.0+i0.0  0.0+i0.0   0.0+i0.0  1.0+i-0.0\n\
         \0.0+i0.0  0.0+i0.0  0.0+i0.0  0.0+i0.0  0.0+i0.0  0.0+i0.0  1.0+i-0.0   0.0+i0.0\n"

main :: IO ()
main = unless (show (toffoli'') == result) exitFailure

-- Local Variables:
-- dante-target: "test-unitary"
-- End:
