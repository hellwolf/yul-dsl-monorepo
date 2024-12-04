{-# LANGUAGE TemplateHaskell #-}
module Data.TupleN
  ( Solo
  -- | Define TupleNtoNP closed type family using Template Haskell.
  , TupleNtoNP
  -- | Convert a TupleN to its corresponding NP.
  , fromTupleNPToNP
  , NPtoTupleN
  ) where

-- base
import           Control.Monad           (replicateM)
-- ghc-experimental
import           Data.Tuple.Experimental (Solo)
-- template-haskell
import qualified Language.Haskell.TH     as TH
--
import           Data.SimpleNP


do
  tfName <- TH.newName "TupleNtoNP"
  tfDec <- do
    tfArg <- TH.newName "t"
    TH.closedTypeFamilyD tfName [TH.plainTV tfArg] TH.noSig Nothing
           ( map (\n -> do
                     xs <- fmap TH.varT <$> replicateM n (TH.newName "x")
                     TH.tySynEqn Nothing
                       (TH.conT tfName `TH.appT` foldl TH.appT (TH.tupleT n) xs)
                       (TH.conT ''NP `TH.appT` foldr (TH.appT . TH.appT TH.promotedConsT) TH.promotedNilT xs)
                  ) [0..64]
           )
  clsInstsDec <- do
    clsName <- TH.newName "FromTupleNPtoNP"
    fnName <- TH.newName "fromTupleNPToNP"
    clsArg <- TH.newName "a"
    cls <- TH.classD (pure []) clsName [TH.plainTV clsArg] []
           [TH.sigD fnName (TH.arrowT `TH.appT` TH.varT clsArg `TH.appT` (TH.conT tfName `TH.appT` TH.varT clsArg))]
    insts <- mapM (\n -> do
                      xs <- replicateM n (TH.newName "x")
                      TH.instanceD (pure [])
                        (TH.conT clsName `TH.appT` foldl TH.appT (TH.tupleT n) (map TH.varT xs))
                        [TH.funD fnName [ TH.clause
                                          [TH.tupP (map TH.varP xs)]
                                          -- NormalB (ConE Main.Nil)
                                          -- NormalB (InfixE (Just (VarE x_9)) (ConE Main.:*) (Just (ConE Main.Nil)))
                                          -- NormalB (InfixE (Just (VarE x1_12)) (ConE Main.:*) (Just (InfixE (Just (VarE x2_13)) (ConE Main.:*) (Just (ConE Main.Nil)))))
                                          (TH.normalB $
                                            foldr
                                            (\a b -> TH.infixE (Just a) (TH.conE '(:*)) (Just b))
                                            (TH.conE 'Nil) (map TH.varE xs))
                                          []
                                        ]
                        ]
                 ) [0..64]
    pure $ [ cls ] ++ insts
  pure $ [ tfDec ] ++ clsInstsDec

do
  tfname <- TH.newName "NPtoTupleN"
  tfDec <- do
    x <- fmap TH.varT (TH.newName "x") -- for Solo
    tfArg <- TH.newName "t"
    TH.closedTypeFamilyD tfname [TH.plainTV tfArg] TH.noSig Nothing (
      -- Special equations for Unit and Solo
      [ TH.tySynEqn Nothing (TH.conT tfname `TH.appT` (TH.conT ''NP `TH.appT` TH.promotedNilT)) (TH.tupleT 0)
      , TH.tySynEqn Nothing (TH.conT tfname `TH.appT` (TH.conT ''NP `TH.appT` (TH.appT TH.promotedConsT x `TH.appT` TH.promotedNilT))) x
      ] ++ (
        -- 2+ tuples
          map (\n -> do
                  xs <- fmap TH.varT <$> replicateM n (TH.newName "x")
                  TH.tySynEqn Nothing
                    (TH.conT tfname `TH.appT`
                     (TH.conT ''NP `TH.appT` foldr (TH.appT . TH.appT TH.promotedConsT) TH.promotedNilT xs))
                    (foldl TH.appT (TH.tupleT n) xs)
              ) [2..64]
          )
      )
  pure [ tfDec ]
