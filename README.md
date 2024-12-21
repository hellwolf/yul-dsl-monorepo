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

Compatible & Extensible Type System
-----------------------------------

> [!NOTE]
>
> These include [Ethereum contract ABI specification](https://docs.soliditylang.org/en/latest/abi-spec.html)
> implemented in as *core types*, their *type extensions*, including *dependently typed extensions*.

(TODO move this distracting big table to its own documentation, instead.)

| ABIType Instances   | [ABICoreType]   | Name (Selector Name)                  | Examples               |
|---------------------|-----------------|---------------------------------------|------------------------|
| *(core types)*      |                 |                                       |                        |
| NP xs               | xs'             | N-ary products ((T1, ... Tn))         | INT 1 :* true :* Nil   |
| BOOL                | [BOOL']         | Boolean (bool)                        | true, false            |
| INTx s n            | [INTx' s n]     | Fixed-precision integers (int?/uint?) | -1, 0, 42, 0xffff      |
| ADDR                | [ADDR']         | Ethereum addresses (address)          | constAddr "#0xABC5..." |
| BYTESn n            | [BYTESn']       | Binary type of n bytes (bytes?)       |                        |
| BYTES               | [BYTES']        | Packed byte arrays (bytes)            | TODO                   |
| ARRAY a             | [ARRAY' a]      | Arrays (T[])                          | TODO                   |
| FIXx s m n          | [FIX m n]       | Fixed-point decimal numbers (fixed)   | TODO                   |
| *(extended types)*  |                 |                                       |                        |
| U8, U16, ... U256   | [INTx' False n] | Aliases of unsigned integers          | (see INTx)             |
| I8, I16, ... I256   | [INTx' True n]  | Aliases of signed integers            | (see INTx)             |
| B1, B2, ... B32     | [BYTESn n]      | Aliases of byte arrays                | (see BYTESn)           |
| REF a w             | [B32']          | Memory or storage references          | TODO                   |
| MAYBE a             | [MAYBE' a]      | Maybe a value                         | TODO                   |
| FUNC c sel          | [U192']         | Contract function pointer             | TODO                   |
| (a, b)              | [a', b']        | Tuples                                | (a, b)                 |
| TUPLEn n            | [a1', ... an']  | Tuples of N-elements                  | (), a, (a, b, c)       |
| STRUCT lens_xs      | xs'             | Struct with lenses                    | TODO                   |
| STRING              | [BYTES']        | UTF-8 strings                         | TODO                   |
| MAP a b             | [B32']          | Hash tables, aka. maps                | TODO                   |
| *(dependent types)* |                 |                                       |                        |
| BOOL'd v            | [BOOL']         | Dependent booleans                    | TODO                   |
| INTx'd s n v        | [INTx' s n]     | Dependent integers                    | TODO                   |
| BYTES'd l           | [BYTES']        | Length-indexed byte arrays            | TODO                   |
| ARRAY'd a l         | [ARRAY' a]      | Length-indexed arrays                 | TODO                   |
| STRING'd v          | [BYTES']        | Dependent strings                     | TODO                   |

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

TODO.

Linear Safety For Side Effects
------------------------------

TODO.

Foundry Integration
-------------------

TODO.

------------------------------------------------------------------------------------------

Packages
========

- *eth-abi* - Ethereum contract ABI specification in Haskell
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

**TODOs for 0.1.0.0**

Headline Features

- eth-abi
  - CoreType:
    - [ ] ARRAY
  - ExtendedTypes:
    - [ ] TUPLEn
    - [ ] REF
- yul-dsl
  - Value primitives:
    - [ ] `YulNum (Maybe a)`, safe numerical operations.
    - [ ] `YulCast`, casting values between value types.
  - Control flow primitives:
    - [ ] `YulMapArray`, tight loop over an array.
    - [ ] `YulLen`, array length.
  - Non pure primitives:
    - [ ] `YulCall`, external function calls.
  - Evaluator
    - [ ] Support all `YulDSL` data constructors.
  - Function Gen:
    - [ ] Change the logic to delay code gen until inner layer requires it.
  - Object builder:
    - [ ] dispatcher builder with full dispatcher calldata codec support.
- yul-dsl-linear-smc
  - [ ] Complete the Num classes: mul, abs, sig, etc.
  - [ ] Prelude curation
- yol-suite
  - Software distributions:
    - [ ] Nix flake
    - [ ] Rudimentary github dev console
- yolc
  - Cabal build system integration
    - [ ] better YOLSuite build sharing.
  - Project Builder
  - Contract verification support:
    - [ ] Basic stunt contract generator.

**TODOs for 0.2.0.0**

- eth-abi
  - CoreType
    - [ ] BYTESn, additional operations
    - [ ] BYTES
  - ExtendedType
    - [ ] STRING
    - [ ] FUNC
    - [ ] STRUCT with named fields
  - ABITypeCodec
    - [ ]  Compatibility with the solidity abi-spec
- yul-dsl
  - Pure value primitives:
    - [ ] `YulAbi{Enc,Dec}`, contracts ABI serialization.
  - CodeGen core:
    - [ ] Fn autoId (instead of using yulCatDigest.)
  - Object builder:
    - [ ] constructor support.
- yol-suite
  - Software distributions:
    - [ ] better github dev console
    - [ ] yolc.dev playground
- yolc
  - Project builder
    - Contract size strategy:
      - [ ] Manual logic split & dispatching,
      - [ ] Auto logic split.
    - Deployment types:
      - [ ] Factory contract,
      - [ ] Shared library.
    - Upgradability:
      - [ ] Singleton upgradability,
      - [ ] Beacon Upgradability.
  - Contract verification support
    - [ ] Full stunt contract generator.
  - CLI
    - [ ] Use 'THSH' to mix shell scripting and publish its haskell binary.
- attila
  - Test Pipeline: `attila test`
    - [ ] QuickCheck integration using Eval monad.
    - [ ] Foundry testing integration using stunt contract.
  - Deployment Pipeline: `attila deploy`
    - [ ] Deploy the program (program is an unit of deployment.)
    - [ ] Etherscan verification pipeline.
- drwitch
  - ...

**Big Features In Research**

- Liquid haskell integration.
- Dependently extended types.
        - Portable YulDSL artifact for non-Haskell language embedding and cross-languages modules.
