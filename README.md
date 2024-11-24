YOLC - Programming Solidity/Yul in Advanced Haskell
===================================================

YulDSL provides an EDSL called 'YulDSL' for transpiling Haskell code to Solidiy/Yul code.

Additionally, the package uses a technique called "evaluating linear functions to symmetric monoidal categories
(Jean-Philippe Bernardy and Arnaud Spiwack)" to provide an ergonomic linear functions (nicknamed "lolipop" functions,
hence the project name) programming environment for the 'YulDSL'.

Furthermore, the 'YulDSL' has its portable artifact form (WORK-IN-PROGRESS), which opens the door for other frontends (a
visual programming interface or other principled programming languages) to produce and share 'YulDSL' as portable
modules with each other.

Motivation
----------

> They (the programming languages) also differ in physical appearance, and more important, in logical structure. The
> question arises, do the idiosyncracies reflect basic logical properties of the situations that are being catered for?
> Or are they accidents of history and personal background that may be obscuring fruitful developments?

— The Next 700 Programming Languages, P. J. Landin

* Many emerging ecosystems have specialized programming languages to work on them. The Solidity programming language,
  which is created for Ethereum Virtual Machine (EVM) related ecosystems, is one example.

* These new languages don't serve any purpose other than programming for its particular ecosystem.

* In the meantime, programming language theories have continued advancing, especially in type theory and category
  theory.

* Amongst these languages, a few have shown to be capable of embedding a domain specific language (EDSL) for a wide
  range of problems without having to build a new programming language toolchain.

* Gratefully, the published work by Jean-Philippe Bernardy and Arnaud Spiwack provides a practical engineering toolkit
  that provides an ergonomic programming environment for such EDSLs using linear-typed Haskell.

* In the quest to provide an advanced, purely functional high-level programming language to the EVM ecosystem, the author
  embarked on a journey into creating this program known as **yolc**.

-----

STILL WORK IN PROGRESS
----------------------

> [!IMPORTANT]
>
> Good news! After pausing for the good part of 2024 due to business reason, I am back to it. As of 2024 October, the
> end-to-end is working and I have adjusted the roadmap and planned an exciting type system for the first release!
>
> Contact me if you are interested in testing this project out soon!

Features
========

Ethereum.ContractABI.Types
--------------------------

> [!NOTE]
>
> These include [Ethereum contract ABI specification](https://docs.soliditylang.org/en/latest/abi-spec.html)
> implemented in as *core types*, their *type extensions*, including *dependently typed extensions*.

| ABIType Instances   | [ABICoreType]     | Name                         | Examples             |
|---------------------|-------------------|------------------------------|----------------------|
| *(core types)*      |                   |                              |                      |
| NP xs               | xs'               | N-ary products               | INT 1 :* true :* Nil |
| BOOL                | [BOOL']           | Boolean                      | true, false          |
| INTx s n            | [INTx' s n]       | Fixed-precision integers     | -1, 0, 42, 0xffff    |
| ADDR                | [ADDR']           | Ethereum addresses           | #0xABC5...290a       |
| BYTESn n            | [BYTESn']         | Binary type of n bytes       |                      |
| BYTES               | [BYTES']          | Packed byte arrays           | TODO                 |
| ARRAY a             | [ARRAY' a]        | Arrays                       | TODO                 |
| FIXx s m n          | [FIX m n]         | Fixed-point decimal numbers  | TODO                 |
| *(extended types)*  |                   |                              |                      |
| U32, ..., U256      | [INTx' False n]   | Aliases of unsigned integers | (see INTx)           |
| I32, ..., I256      | [INTx' True n]    | Aliases of signed integers   | (see INTx)           |
| B1, B2, .. B32      | [BYTESn n]        | Aliases of byte arrays       | (see BYTESn)         |
| REF a w             | [B32']            | Memory or storage references | TODO                 |
| MAYBE a             | [MAYBE' a]        | Maybe a value                | TODO                 |
| FUNC c sel          | [U192']           | Contract function pointer    | TODO                 |
| (a, b)              | [a', b']          | Tuples                       | (a, b)               |
| TUPLEn n            | [a1', a2' .. an'] | Tuples of N-elements         | (), a, (a, b, c)     |
| STRUCT lens_xs      | xs'               | Struct with lenses           | TODO                 |
| STRING              | [BYTES']          | UTF-8 strings                | TODO                 |
| MAP a b             | [B32']            | Hash tables, aka. maps       | TODO                 |
| *(dependent types)* |                   |                              |                      |
| BOOL'd v            | [BOOL']           | Dependent booleans           | TODO                 |
| INTx'd s n v        | [INTx' s n]       | Dependent integers           | TODO                 |
| BYTES'd l           | [BYTES']          | Length-indexed byte arrays   | TODO                 |
| ARRAY'd a l         | [ARRAY' a]        | Length-indexed arrays        | TODO                 |
| STRING'd v          | [BYTES']          | Dependent strings            | TODO                 |


TODOs:

- CoreTypes:
  - BYTESn
    - all supported operations
  - BYTES
    - ...
  - ARRAY
    - ...
- ExtendedTypes:
  - REF
  - FUNC
  - Maybe
  - STRING
  - Tuple, TUPLEn
  - STRUCT

YulDSL
------

> [!NOTE]
>
> YulDSL, a DSL for Solidity/Yul.

## YulCat

> [!NOTE]
>
> DSL of Yul defined with categories.

TODOs:

- Safety:
  - [ ] `P'L (v :: Nat) r a`, linearly-safety with data generation versioning tag "v".
- Value primitives:
  - [ ] `YulAbi{Enc,Dec}`, contracts ABI serialization.
  - [ ] `YulCast`, casting values between value types.
- Control flow primitives:
  - [ ] `YulMap`, tight loop over an array.
  - [ ] `YulLen`, array length.
  - [ ] `YulPat`, pattern matching.
- Effects:
  - [ ] `YulSet, YulSPut`, storage operations.
  - [ ] `YulCall`, external function calls.
- Utilities:
  - [x] `(>.>)` and `(<.<)` operators for the directional morphisms.
  - [x] MPOrd class
  - [x] IfThenElse class
  - [x] Num instance
  - [-] Show instance

## Fn

> [!NOTE]
>
> Function object for a YulCat.

```haskell
-- define a pure value function
foo3 = fn @(Maybe U8 -> Maybe U8 -> Maybe U8 -> Maybe U8) "id"
       (\a b c -> a + b + c)

-- call other pure value function
call3 = fn @(Maybe U8 -> Maybe U8) "id"
  (\a -> call foo3 a a a)
```

## YulObject

> [!NOTE]
>
> Yul object builder. Yul object specification can be found from [solidity
> documentation](https://docs.soliditylang.org/en/latest/yul.html#specification-of-yul-object).

## Eval

> [!NOTE]
>
> Evaluation of a YulCat semantically.

TODOs:

- [ ] :L: Support all `YulDSL` data constructors.

## CodeGens.YulGen

> [!NOTE]
>
> Generate yul code for Fn, YuLCat, and YulObject.

TODOs:

- Function Gen:
  - [ ] Change the logic to delay code gen until inner layer requires it.
- CodeGen core:
  - [ ] Fn autoId (instead of using yulCatDigest.)
- Object builder:
  - [ ] dispatcher builder with full dispatcher calldata codec support.
  - [ ] constructor support.

## CodeGens.Diagrams

> [!NOTE]
>
> Generate diagrams using Haskell diagrams package.

YulDSL Linear-SMC Frontend
--------------------------

TODOs:

- Prelude curation.

YOL Suite
---------


> [!IMPORTANT]
>
> - YOL stands for *Yet Original Language*.
> - YOL suite is *For the New Pioneer* of EVM application development.

## yolc: the evil twin of solc

TODOs:

- Project Builder
  - Manifest Builder:
    - [x] Single-file output mode.
    - [ ] Interface file generation.
    - [ ] Better error messages.
  - Deployment types:
    - [x] Singleton contract.
    - [ ] Factory contract.
    - [ ] Shared library.
  - Upgradability patterns:
    - [ ] Grandfatherly upgradable.
    - [ ] Full upgradable.
    - [ ] Simple library template.
  - Contract verification support:
    - [ ] Stunt contract generator.
    - [ ] Multi-files output mode
- CLI: `yolc [options] yol_module_spec...`
  - Build Pipeline:
    - [ ] Better YOLSuite build sharing.
  - Output modes:
    - [x] Show output mode.
    - [x] Yul output mode.
    - [ ] Haskell diagrams output mode.
  - Compiler Modes:
    - [x] `symbol   :: FnCat a b`, fnMode
    - [x] `object   :: YulObject`, objectMode
    - [x] `manifest :: Manifest`, projectMode

## attila: who wields the foundry, forges his path

- Test Pipeline: `attila test`
  - [ ] QuickCheck integration using Eval monad.
  - [ ] Foundry testing integration using stunt contract.
- Deployment Pipeline: `attila deploy`
  - [ ] Deploy the program (program is an unit of deployment.)
  - [ ] Etherscan verification pipeline.

## drwitch: who persuades the tyrant, shapes our history

> [!NOTE]
> This should be the counter part of the "cast" from foundry.

## Software Distribution

- [ ] :M: Nix flake.

Future Plans
============

- Liquid Haskell integration.
- YulDSL artifact.
- Effect system.
