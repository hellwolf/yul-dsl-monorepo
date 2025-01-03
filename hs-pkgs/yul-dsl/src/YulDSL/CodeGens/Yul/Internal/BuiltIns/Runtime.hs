{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
module YulDSL.CodeGens.Yul.Internal.BuiltIns.Runtime (exports, prelude) where
-- yul-dsl
import YulDSL.Core
-- text
import Data.Text.Lazy                               qualified as T
--
import YulDSL.CodeGens.Yul.Internal.BuiltInRegistra
import YulDSL.CodeGens.Yul.Internal.Variable



------------------------------------------------------------------------------------------------------------------------
-- Prelude
------------------------------------------------------------------------------------------------------------------------

raw_boolean_operators =
  [ const_builtin  "eq" []
  , const_builtin "neq" [ "function neq(a, b) -> r { r := iszero(eq(a, b)) }" ]
  , const_builtin  "gt" []
  , const_builtin  "lt" []
  , const_builtin  "le" [ "function le(a, b) -> r { r := iszero(gt(a, b)) }" ]
  , const_builtin  "ge" [ "function ge(a, b) -> r { r := iszero(lt(a, b)) }" ]
  , const_builtin "sgt" []
  , const_builtin "slt" []
  , const_builtin "sle" [ "function sle(a, b) -> r { r := iszero(sgt(a, b)) }" ]
  , const_builtin "sge" [ "function sge(a, b) -> r { r := iszero(slt(a, b)) }" ]
  ]

revert_forward_1 = const_builtin "revert_forward_1"
  [ "function revert_forward_1() {"
  , " let pos := allocate_unbounded()"
  , " returndatacopy(pos, 0, returndatasize())"
  , " revert(pos, returndatasize())"
  , "}"
  ]

allocate_unbounded =  const_builtin "allocate_unbounded"
  [ "function allocate_unbounded() -> memPtr { memPtr := mload(64) }" ]

finalize_allocation = const_builtin_with_deps "finalize_allocation"
  [ "function finalize_allocation(memPtr, size) {"
  , " size := and(add(size, 31), not(31)) // round_up_to_mul_of_32"
  , " let newFreePtr := add(memPtr, size)"
  , " // protect against overflow"
  , " if or(gt(newFreePtr, 0xffffffffffffffff), lt(newFreePtr, memPtr)) { panic_error_0x41() }"
  , " mstore(64, newFreePtr)"
  , "}"
  ]
  [ "panic_error_0x41" ]

------------------------------------------------------------------------------------------------------------------------
-- Memory management
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- Exceptions
------------------------------------------------------------------------------------------------------------------------

-- TODO: rename to `const_revert0_c_
revert0 = mk_builtin "__revert_c_" $ \part full ->
  let types = decodeAbiCoreTypeCompactName part
      vars  = gen_vars (length types)
  in ( [ "function " <> T.pack full <> "() -> " <> spread_vars vars  <> " {"
       , " revert(0, 0)"
       , "}"
       ]
     , [])

panic_errors = mk_builtin "panic_error_" $ \part full ->
  ( [ "function " <> T.pack full <> "() {"
    -- `cast sig 'Panic(uint256)'` == 0x4e487b71
    , "  mstore(0, 0x4e487b71" <> T.pack (replicate 56 '0') <> ")"
    , "  mstore(4, " <> T.pack part <> ")"
    , "  revert(0, 0x24)"
    , "}"
    ]
  , [])

------------------------------------------------------------------------------------------------------------------------
-- Dispatcher
------------------------------------------------------------------------------------------------------------------------

dispatcher_builtins =
  [ const_builtin_with_deps "__dispatcher_dependencies" [] -- it is a pseudo builtin
    [ "__selector"
    ]

  , const_builtin "__selector"
    [ "function selector() -> s {"
    , " s := div(calldataload(0), 0x100000000000000000000000000000000000000000000000000000000)"
    , "}"
    ]
  ]

prelude =
  raw_boolean_operators ++
  [ revert_forward_1
  , allocate_unbounded
  , finalize_allocation
  ]

exports =
  prelude ++
  [ revert0
  , panic_errors
  ] ++
  dispatcher_builtins
