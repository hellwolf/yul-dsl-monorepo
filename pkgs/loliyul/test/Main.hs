{-# LANGUAGE LinearTypes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

import           Control.Category.Linear
import           Data.Proxy              (Proxy (..))

import           LoliYul.Yul

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
