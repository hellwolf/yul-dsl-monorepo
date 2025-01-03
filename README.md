Yolc - A Safe, Expressive, Fun Language for Ethereum
====================================================

The main motivation behind Yolc is to strike a balance between the following values for building Ethereum smart
contracts:

*Safe*

Yolc is purely functional with linear type safety, made for the Ethereum virtual machine.

> What does *purely functional linear type safety* mean here? Read more [here](#).

*Expressive*

Yolc embeds itself in the Haskell language before being compiled into Solidity/Yul code.

> Why does *expressiveness* matter? Read more [here](#).

*Fun*

Yolc allows you to write safe code in production, a joyful experience for super coders.

> Check out these [example codes](#).

> [!TIP]
>
> Yolc is a compiler program for "YulDSL/Haskell". YulDSL is a domain-specific language (DSL) based on [category
> theory](https://category-theory.org/) for [Solidity/Yul](https://soliditylang.org/). YulDSL can be embedded in
> different languages, with "YulDSL/Haskell" being the first of its kind. Curiously, the name "yolc" sounds similar to
> "solc", the compiler program for "Solidity/Yul".
>
> Do not worry if you don't understand some of these concepts, you can start with Yolc right away and have a rewarding,
> fun experience writing safer production smart contracts. However, if you do feel adventurous and want to delve into
> the inner workings of YulDSL, read [here](./hs-pkgs/yul-dsl/README.md).

> [!CAUTION]
>
> 🚧 While this project is still work in progress 🚧, the good news is after pausing for the good part of 2024 due to
> business reason, I am back to it. As of 2024 October, the end-to-end is working and I have adjusted the roadmap and
> planned an exciting type system for the first release!
>
> Contact me at info@yolc.dev if you are interested in testing this project out soon!

------------------------------------------------------------------------------------------

Features
========

Ethereum-Compatible & Extensible Types
--------------------------------------

> [!NOTE]
>
> These include [Ethereum contract ABI specification](https://docs.soliditylang.org/en/latest/abi-spec.html)
> implemented in as *core types*, their *type extensions*, including *dependently typed extensions*.

Unlike solidity, and to accommodate Haskell lexical rules, types are all in capitalize letters:

* Boolean type `BOOL`, and its values `true`, `false`.
* Address type `ADDR`.
* Integers types: `I8`, `I16`, ... `I256`; `U8`, `U16`, ... `U256`.
* etc.

Full table of the types implemented and planned can be found [here](./hs-pkgs/eth-abi/README.md).

Expressive Pure Functions
-------------------------

**Haskell Native Syntax**

TODO.

**Currying Function Definition**

```haskell
-- define a pure value function
foo3 = fn @(Maybe U8 -> Maybe U8 -> Maybe U8 -> Maybe U8) "foo3"
  \a b c -> a + b + c

-- call other pure value function
call3 = fn @(Maybe U8 -> Maybe U8) "call3"
  \a -> call foo3 a a a
```

**Pattern Matching**

```haskell
add_maybe_int96_with_default = fn @(I96 -> I96 -> I96 -> I96) "add_maybe_int96_with_default"
  \x y def -> match (inCase (Just x) + inCase (Just y)) \case
    Nothing -> def
    Just z  -> z
```

Linear Safety For Side Effects
------------------------------

TODO.

Foundry Integration
-------------------

TODO.

------------------------------------------------------------------------------------------

Packages
========

- [*eth-abi*](./hs-pkgs/eth-abi/README.md) - Ethereum contract ABI specification in Haskell
- [*yul-dsl*](./hs-pkgs/yul-dsl/README.md) - A DSL for Solidity/Yul
- [*yul-dsl-linear-smc*](./hs-pkgs/yul-dsl-linear-smc/README.md) - Embedding YulDSL in Haskell Using Linear Types
- [*yol-suite*](./hs-pkgs/yol-suite/README.md) - A Collection of YulDSL Programs for the New Pioneer of Ethereum Smart
  Contracts Development
  - **yolc**: the evil twin of "solc"; this is the compiler program for "YulDSL/Haskell".
  - **attila**: who wields the foundy, forges his path; this is the counter part of the "forge" from
    [foundry](https://github.com/foundry-rs/foundry).
  - **drwitch**: who persuades the tyrant, shapes our history; this is the counter part of the "cast" from
    [foundry](https://github.com/foundry-rs/foundry).

------------------------------------------------------------------------------------------

TODOs & Future Plans
====================

> [!WARNING]
>
> YOU DON'T NEED TO LOOK AT THIS DIRTY LAUNDRY!
>
> Legend for items:
> * ⭐,🌟 - Highlighted feature.
> * 🟢 - Planned and low uncertainty;
> * 🟠 - Planned with some design decisions to be made;
> * 🔴 - Likely deferred to the future versions;
> * ❓ - To be reviewed.

**TODOs for 0.1.0.0**

- eth-abi
  - CoreType
    - [x] NP (N-ary Product, type-level recursive-friendly alternative to tuple type)
    - [x] BOOL
    - [x] INTx s n
    - [x] BYTESn
    - [x] ADDR
- yul-dsl
  - YulCat
    - Value functions
      - [x] All integer number types: U8, U16.. U256; I8, I16, .. I256.
      - [x] All fixed size bytes types: B1, B2... B32.
      - [ ] 🚧 ⭐ Safe number handling with
        1. checked number operations,
        2. number operations over optional values,
        3. pattern matching of optional (Haskell `Maybe` type) values
        4. type-safe upCast, and safeCast to optional values.
        - [ ] 🟢 `YulCast`, casting values between value types.
        - [x] 🟢 All `Num` and `Maybe Num` built-ins
      - [x] yulKeccak256 for supported types.
    - Side Effects
      - [x] `YulSGet`, `YulSPut` for raw storage operations.
      - [x]`YulCall`, external function calls.
    - Exceptions
      - [x] `revert0`, solidity-equivalent of `revert()`
    - Control flows
      - [x] Haskell native if-then-else expression through "RebindableSyntax".
      - [x] ⭐ Pattern matching support of optional number values.
    - Yul Object
      - [x] Function export modifiers resembling solidity: `pureFn, staticFn, externalFn`.
    - Type safety
      - [x] Type-level purity classification: `IsEffectNotPure, MayEffectWorld`.
  - Built-in Yul Functions Infrastructure
    - [ ] 🚧 🟢 Full arithmetic support
    - [ ] 🚧 🟢 Full ABICodec support
  - Working with pure effect
    - [x] Build pure functions `fn`. ⚠️ This will be replaced with `$fn`.
    - [x] Call pure functions `callFn`.
  - CodeGen
    - [x] Yul code generator for any YulCat
    - [x] Yul object dispatcher generator for exported functions.
  - Evaluator
    - `evalFn` to evaluate `Fn` (single YulCat value styled as a function) value.
- yul-dsl-linear-smc
  - [x] 🌟🌟🌟 Linear safety for side effects
    - [x] Compile expression sof linear _data ports_ to YulCat
    - [x] Working with _versioned data port_ through `YulMonad`, a "Linearly Versioned Monad."
    - [x] Build linear functions with `lfn`. ⚠️ This will be replaced with `$yulMonadV, $yulMonadP`.
    - [x] Call functions linearly with `callFn'l`, `callFn'lpp`.`
  - Working with _data ports_
    - [x] 🟢 match'l for pattern matching data ports.
    - [ ] 🟢 ifThenElse through pattern matching on BOOL data port.
    - [ ] 🟢 Num classes for data ports: mul, abs, sig, etc.
  - Working with storage:
    - [ ] 🟢 Low-level storage functions for input data ports, `sget, sput`.
- yol-suite
  - YOLC
    - Singleton program factory
      - [x] Program interface, e.g. `interface IERC20Program`
      - [x] Program factory, e.g. `function createERC20Program()`
    - Contract verification support
      - [ ] 🟢 EIP-1967 compatible "stunt contract" generator. A stunt contract includes both:
        1. the program's interface necessary to interact with the program via EIP-1967-aware explorers,
        2. a copy of Haskell main source code in a block of solidity comments.
  - CLI
    - [x] ⭐ `yolc`, a MVP in shells script, prepares YOLC project and invoke YOLC builder.
  - Developer communication
    - [x] Annotated ERC20 demo
  - Software distributions
    - [x] Nix flake
    - [ ] 🟢 Rudimentary github dev console

**TODOs for 0.2.0.0**

- eth-abi
  - CoreType
    - [ ] 🟠 ARRAY a
    - [ ] 🟠 BYTES
    - [ ] 🟠 STRING
  - ExtendedType
    - [ ] 🟠 REF, storage or memory reference
    - [ ] 🟠 SELECTOR
    - [ ] 🟠 TUPLEn, STRUCT with named fields, etc.
  - ABICodec
    - [ ] 🟢 Compatibility with the solidity abi-spec
- yul-dsl
  - YulCat
    - Value functions
      - [ ] 🟢 yulKeccak256 evaluation function using ABICodec from eth-abi.
      - [ ] 🟢 array length built-in.
      - [ ] 🟠 Maybe support of non word values.
    - Exceptions
      - [ ] 🟢 `revertWithError`
      - [x] 🟢 `revertWithMessage`
    - Control flows
      - [ ] 🟠 `YulMapArray`, tight loop over an array.
    - Yul object
    - Side Effects
      - [ ] 🟢 `YulStaticCall`, static external calls.
      - [ ] 🟢 `YulDelegateCall`, delegate external calls.
    - Type safety
      - ❓ further encode total functions in type
  - Working with pure effect
    - [ ] 🟠 `$fn` template haskell for generating automatic unique function id.
  - CodeGen
    - Function Gen:
      - [ ] 🟠 Fix the implementation for all embeddable values.
    - Object builder:
      - [ ] 🟠 constructor support.
  - Evaluator
    - [ ] 🟢 handling exception
    - [ ] 🟢 test coverage, and check against foundry results
- yul-dsl-linear-smc
  - Working with _versioned data port_ through `YulMonad`, a "Linearly Versioned Monad."
    - [ ] 🟢 Build YulMonad functions: `$yulMonadV` for versioned inputs, and `$yulMonadP` for pure inputs.
    - [ ] 🟠 Storage functions working with `Referenceable` types.
- yol-suite
  - YOLC
    - [ ] 🟠 Solidity struct generator for types.
    - Advanced program deployment strategy:
      - [ ] 🟠 manual logic split through delegateCall.
      - [ ] 🔴 auto logic split & dispatching,
      - [ ] 🔴 Shared library.
    - Program upgradability:
      - [ ] 🟠 Beacon upgradability.
    - Contract verification support
      - [ ] 🔴 Full stunt contract generator.
  - CLI
    - [ ] 🔴 Use 'THSH' to mix shell scripting and publish its haskell binary.
  - Developer communication
  - Software distributions
    - [ ] better github dev console
    - [ ] yolc.dev playground
- attila
  - Test Pipeline: `attila test`
    - [ ] 🟠 Foundry testing integration using stunt contract.
    - [ ] 🔴 QuickCheck integration using Eval monad.
  - Deployment Pipeline: `attila deploy`
    - [ ] 🟠 Deploy the program (program is an unit of deployment.)
    - [ ] 🔴 Etherscan verification pipeline.
- drwitch
  - ...

**Big Features In Research**

- Linearly Versioned Monad, comparing to other resource management method including monadic regions, CoDensity, etc.
- Liquid haskell integration.
- Dependently extended types.
- Portable YulDSL artifact for non-Haskell language embedding and cross-languages modules.
