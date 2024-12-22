{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
module YulDSL.CodeGens.Yul.Internal.BuiltIns.Runtime (exports) where
-- text
import qualified Data.Text.Lazy                               as T
--
import           YulDSL.CodeGens.Yul.Internal.BuiltInRegistra


------------------------------------------------------------------------------------------------------------------------
-- Memory management
------------------------------------------------------------------------------------------------------------------------

allocate_unbounded =  const_builtin "__allocate_unbounded"
  "function __allocate_unbounded() -> memPtr { memPtr := mload(64) }"

------------------------------------------------------------------------------------------------------------------------
-- Dispatcher
------------------------------------------------------------------------------------------------------------------------

dispatcher_builtins =
  [ const_builtin_with_deps "__dispatcher_dependencies" "" -- it is a pseudo builtin
    [ "__allocate_unbounded"
    , "__selector"
    ]

  , const_builtin "__selector" $
    T.unlines
    [ "function selector() -> s \n"
    , "{ s := div(calldataload(0), 0x100000000000000000000000000000000000000000000000000000000) }"
    ]
  ]

------------------------------------------------------------------------------------------------------------------------
-- Exceptions
------------------------------------------------------------------------------------------------------------------------

panic_errors = mk_builtin "panic_error_" $ \part full ->
  (T.unlines
    [ "function " <> T.pack full <> "() {"
    -- `cast sig 'Panic(uint256)'` == 0x4e487b71
    , "  mstore(0, 0x4e487b71" <> T.pack (replicate 56 '0') <> ")"
    , "  mstore(4, " <> T.pack part <> ")"
    , "  revert(0, 0x24)"
    , "}"
    ]
  , [])

exports =
  [ allocate_unbounded
  ] ++
  [ panic_errors
  ] ++
  dispatcher_builtins
