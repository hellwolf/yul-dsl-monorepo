{-# LANGUAGE OverloadedStrings #-}

module YulDSL.CodeGens.Yul.Internal.CodeFormatters where

import           Data.Functor.Identity (Identity (runIdentity))
--
import qualified Data.Text.Lazy        as T


-- | Code is text.
type Code = T.Text

-- | Indentation formatter.
type Indenter = Code -> Code

-- | Add one level of indentation to a text.
add_indent :: Indenter
add_indent s = " " <> s

-- | Add one level of indentation to an indenter.
indent :: Indenter -> Indenter
indent ind s = add_indent (ind s)

-- | Initial line indentation.
init_ind :: Indenter
init_ind s = s <> "\n"

-- | Wrap monadic code gen in a pair of curly brackets.
cbracket_m :: Monad m => Indenter -> Code -> (Indenter -> m Code) -> m Code
cbracket_m ind prefix codegen = codegen (indent ind) >>=
                                \code -> pure (ind (prefix' <> "{" ) <> code <> ind "}")
  where prefix' = if prefix == "" then "" else prefix <> " "

-- | Wrap code in a pair of curly brackets.
cbracket :: Indenter -> Code -> (Indenter -> Code) -> Code
cbracket ind prefix codegen = runIdentity $ cbracket_m ind prefix (pure . codegen)

-- | Wrap a one liner in a pair of curly brackets.
cbracket1 :: Indenter -> Code -> Code -> Code
cbracket1 ind prefix oneliner = cbracket ind prefix ($ oneliner)
