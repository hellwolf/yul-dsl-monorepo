Yolc - A Safe, Expressive, Fun Language for Ethereum
====================================================

The main motivation behind Yolc is to strike a balance between these values for building Ethereum smart contracts:

*Safe*

Yolc is purely functional with linear type safety, made for the Ethereum virtual machine.

> What does *purely functional linear type safety* mean here? Read more [here](#).

*Expressive*

YulDSL provides an EDSL called 'YulDSL' for transpiling Haskell code to Solidiy/Yul code.

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
  - ExtendedType
    - [ ] 🟠 REF, storage or memory reference
- yul-dsl
  - YulCat
    - Value type functions
      - All integer number types: U8, U16.. U256; I8, I16, .. I256.
      - [-] ⭐ Safe number handling with
        1) checked number operations,
        2) number operations over optional values,
        3) pattern matching of optional (Haskell `Maybe` type) values
        4) type-safe upCast, and safeCast to optional values.
        - [ ] 🟢 `YulCast`, casting values between value types.
        - [x] 🟢 All `Num` and `Maybe Num` built-ins
    - ⭐ Pattern matching support of optional number values.
    - Haskell native if-then-else expression through "RebindableSyntax".
    - yulKeccak256 for supported types.
    - Exception
      - `revert0`, solidity-equivalent of `revert()`
    - Impurity and side effects
      - [x] `YulSGet`, `YulSPut` for raw storage operations.
      - [ ] 🟠 `YulCall`, external function calls.
  - CodeGen
    - [x] Function generator for any YulCat
    - Object generator:
      - [x] Dispatcher generator
    - Yul built-ins generators
      - [-] 🟢 Full arithmetic support
      - [-] 🟢 Full ABICodec support
  - Evaluator
    - `evalFn` to evaluate a single YulCat morphism.
- yul-dsl-linear-smc
  - [x] 🌟🌟🌟 Linear type sefety through Linearly Versioned Monad.
    - [x] 🟠 More storage combinators.
  - [-] ⭐ Compile linear port expressions to yul-dsl
    - [ ] 🟢 ifThenElse for port.
    - [ ] 🟢 Complete the Num classes: mul, abs, sig, etc.
- yol-suite
  - YOLC
    - Singleton program factory
      - [x] Program interface, e.g. `interface IERC20Program`
      - [x] Program factory, e.g. `function createERC20Program()`
    - Contract verification support
      - [ ] 🟢 EIP-1967 compatible "stunt contract" generator. A stunt contract includes both:
            a) the program's interface necessary to interact with the program via EIP-1967-aware explorers,
            b) a copy of Haskell main source code in a block of solidity comments.
  - CLI:
    - [x] ⭐ `yolc`, a MVP in shells script, prepares YOLC project and invoke YOLC builder.
  - Developer communication
    - [ ] 🟠 Annotated ERC20 demo
  - Software distributions
    - [ ] 🟢 Nix flake
      - Rudimentary github dev console

**TODOs for 0.2.0.0**

- eth-abi
  - CoreType
    - [ ] 🟠 ARRAY a
    - [ ] 🟠 BYTES
    - [ ] 🟠 STRING
  - ExtendedType
    - [ ] 🟠 SELECTOR
    - [ ] 🟠 TUPLEn, STRUCT with named fields, etc.
  - ABICodec
    - [ ] 🟢 Compatibility with the solidity abi-spec
- yul-dsl
  - YulCat
    - Value type
    - [ ] 🟢 yulKeccak256 evaluation function using ABICodec from eth-abi
    - Composite type support
      - [x] 🟢 array length built-in.
      - [ ] 🟠 Maybe support of non WordValues
    - Control flow
      - [ ] 🟠 `YulMapArray`, tight loop over an array.
    - Exception
      - [ ] 🟢 `revertWithError`
      - [ ] 🟢 `revertWithMessage`
    - Type safety
      - ❓ further encode partial functions in type
  - CodeGen
    - [ ] ❓ Fn autoId (instead of using yulCatDigest.)
    - Function Gen:
      - [ ] 🟠 Fix the implementation for all embeddable values.
    - Object builder:
      - [ ] 🟠 constructor support.
  - Evaluator
    - [ ] 🟢 handling exception
    - [ ] 🟢 test coverage, and check against foundry results
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
  - Software distributions:
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
