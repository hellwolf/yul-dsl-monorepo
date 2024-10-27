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

YulDSL
------

> [!NOTE]
> YulDSL, a DSL for Solidity/Yul.

## Types

> [!NOTE] These include [Ethereum contract ABI specification](https://docs.soliditylang.org/en/latest/abi-spec.html)
> implemented in Haskell types, higher order types, and dependently typed variants of some.

| ABIType Instances | [ABICoreType]     | Name                         | Examples          |
|-------------------|-------------------|------------------------------|-------------------|
| *core types*      |                   |                              |                   |
|-------------------|-------------------|------------------------------|-------------------|
| ()                | []                | Unit                         |                   |
| BOOL              | [BOOL']           | Boolean                      | true, false       |
| INTx s n          | [INTx' s n]       | Fixed-precision integers     | -1, 0, 42, 0xffff |
| ADDR              | [ADDR'            | Ethereum addresses           | #0xABC5...290a    |
| BYTESn n          | [BYTESn' n]       | Fixed-size byte arrays       | TODO              |
| BYTES             | [BYTES']          | Packed byte arrays           | TODO              |
| ARRAY a           | [ARRAY' a]        | Arrays                       | TODO              |
|-------------------|-------------------|------------------------------|-------------------|
| *derived types*   |                   |                              |                   |
|-------------------|-------------------|------------------------------|-------------------|
| U32, ..., U256    | [INTx' False n]   | Aliases of unsigned integers | (see INTx)        |
| I32, ..., I256    | [INTx' True n]    | Aliases of signed integers   | (see INTx)        |
| B1, B2, .. B32    | [BYTESn n]        | Aliases of byte arrays       | (see BYTESn)      |
| REF a w           | [B32']            | Memory or storage references | TODO              |
| (a, b)            | [a', b']          | Tuples                       |                   |
| NP xs             | xs'               | N-ary products               |                   |
| NT n              | [a1', a2' .. an'] | N-ary tuples                 |                   |
| STRUCT lens_xs    | xs'               | Struct with lenses           | TODO              |
| STRING            | [BYTES']          | UTF-8 strings                | TODO              |
| MAP a b           | [B32']            | Hash tables, aka. maps       | TODO              |
| [REF a]           | [ARRAY' a, U256'] | Lazy-list of array iterators | TODO              |
| FUNC c sel e d    | [BYTES32']        | Contract function pointer    | TODO              |
|-------------------|-------------------|------------------------------|-------------------|
| *dependent types* |                   |                              | TODO              |
|-------------------|-------------------|------------------------------|-------------------|
| BOOL'd v          | [BOOL']           | Dependent booleans           | TODO              |
| INTx'd s n v      | [INTx' s n]       | Dependent integers           | TODO              |
| BYTES'd l         | [BYTES']          | Length-indexed byte arrays   | TODO              |
| ARRAY'd a l       | [ARRAY' a]        | Length-indexed arrays        | TODO              |
| STRING'd v        | [BYTES']          | Dependent strings            | TODO              |

## YulCat

TODOs:

- Safety Features:
  - [ ] :M: `YulCat p a b `, `p :: FnPerm` as the type-Level function permission tag.
- SMC Primitives:
  - [x] Category: `YulId; YulComp, ∘`;
  - [x] Monoidal: `YulProd, ×; YulSwap, σ`;
  - [x] Catesian: `YulFork, ▵; YulExl, π₁; YulExr, π₂; YulDis, ε; YulDup, δ;`
- Control Flow Primitives:
  - [x] `YulEmbed`, embedding constant.
  - [x] :M: `YulITE`, if-then-else.
  - [x] :M: `YulJump`, internal function calls.
  - [ ] :M: `YulCall`, external function calls.
  - [ ] :M: `YulMap, YulFoldl`, control structure for lists.
- Utilities:
  - [x] `(>.>)` and `(<.<)` operators for the directional morphisms.
  - [x] MPOrd class
  - [x] IfThenElse class
  - [x] Num instance
  - [-] Show instance
- Storage Primitives:
  - [ ] :M: `YulView`, for indexed or named position.
  - [ ] :M: `YulGet, YulPut` using `REF`, and remove `YulSet, YulSPut`.

## YulObject

TODOs:

- [ ] :S: Module documentation.

## Eval

TODOs:

- [ ] :L: Support all `YulDSL` constructors.

## CodeGens.YulGen

TODOs:

- CodeGen core:
  - [ ] :S: Fn autoId (instead of using yulCatDigest.)
- Object builder:
  - [ ] 🚧 :XL: dispatcher builder with full dispatcher calldata codec support.
  - [ ] :M: constructor support.

## CodeGens.Diagrams

YulDSL Linear-SMC Frontend
--------------------------

TODOs:

- Multi-style functions:
  - [x] :S: composition of all styles using `(>.>)`.
  - [x] :S: `IfThenElse` typeclass, rebindable if-then-else syntax.
- YulCat combinators:
  - [x] :S: `Num, MPOrd` instances for `YulNum`.
  - [x] :S: `IfThenElse` instance.
  - [x] :M: `vfn, ap'vfn`, value function declaration and application.
- Yul Port combinators:
  - [x] :S: `Num, MPOrd` instances for `YulNum`.
  - [x] :S: `IfThenElse` instance.
  - [x] :M: `lfn, ap'lfn`, linearly-typed function declaration and application.
- Prelude:
  - [ ] :L: Curation.

YOL Suite
---------

TODOs:

- YOL stands for *Yet Original Language*.
- YOL suite is *For the New Pioneer* of EVM application development.

## yolc: the evil twin of solc

TODOs:

- Project Builder
  - Manifest Builder:
    - [x] Single-file output mode.
    - [ ] :M: Better error messages.
  - Deployment types:
    - [x] :S: Singleton contract.
    - [ ] :S: Factory contract.
    - [ ] :S: Shared library.
  - Upgradability patterns:
    - [ ] :S: Grandfatherly upgradable.
    - [ ] :S: Full upgradable.
    - [ ] :S: Simple library template.
  - Contract verification support:
    - [ ] :M: Stunt contract generator.
      - 🔴 Blocked by `mkTypedSelector` support.
    - [ ] :M: Multi-files output mode
- CLI: `yolc [options] yol_module_spec...`
  - Build Pipeline:
    - [ ] :?: Better YOLSuite build sharing.
  - Output modes:
    - [x] Show output mode.
    - [x] Yul output mode.
    - [ ] :M: Haskell diagrams output mode.
  - Compiler Modes:
    - [x] `symbol :: Fn a b`, fnMode
    - [x] `object :: YulObject`, objectMode
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
