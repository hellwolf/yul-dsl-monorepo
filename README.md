YOLC - Programming Solidity/Yul in Linear-typed (Lolipop ⊸) Functions
=====================================================================

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

STILL WORK IN PROGRESS
----------------------

Contact me if you are interested in testing this project out soon!

Features
--------

## YulDSL

- Core
  - ContractABI: Solidity-Contract-ABI-Compatible Types
    - Primitive Types:
      - [x] Simple Static value types: `ADDR`, `BOOL`, `INTx (s :: KnownBool) (n :: KnownNat)`.
      - [ ] :S: `SVALUE`, storage value.
      - [ ] :S: `type BYTESnn = BYTES_N n`, static bytes value type.
      - [ ] :L: `BYTES(l?) n?`, optionally-length-indexed dynamic bytes type.
      - [ ] :M: `STRING`, dynamic utf-8 string type.
      - [ ] :M: `ARRAY(l?) (True|False) n? a, DARR, SARR`, optionally-length-indexed dynamic or static array type.
    - Derivative Types:
      - Tuple Types
        - [x] :L: `:*`, n-ary product.
        - [ ] :L: `(,..)`, solo and n-tuple types for function specification.
      - Function Types:
        - [x] `SEL`, selector data type.
        - [x] `mkTypedSelector, mkRawSelector`.
        - [ ] :M: `sigToSelector`, similar to `cast sig`.
        - [-] :S: `FUNC a b`, external function reference with `FuncStorage` and `FuncEffect` tags.
          - Missing show instance.
      - Lenses:
        - [ ] :M: `a :@ "name"` to name a tuple element.
    - Reference
      - [ ] :M: `data REF = VREF | MREF | SREF SLOC; data SLOC = SLOT s o | SNAME name;`.
    - **Completeness:**
      - [ ] :M: **CLEAN-UP** inline-REPL docs.
  - Storage Data Types:
      - [ ] :M: `LIST(l?) n? a`, optionally-length-indexed singly-linked list type.
      - [ ] :M: `MAP k v`, key-value storage.
  - YulCat
      - [x] `(>.>)` operator for the `YulDSL` morphism left-to-right composition.
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
      - Storage Primitives:
        - [ ] :M: `YulView`, for indexed or named position.
        - [ ] :M: `YulGet, YulPut` using `REF`, and remove `YulSet, YulSPut`.
  - YulObject
    - [ ] :S: Module documentation.
- Eval Monad:
  - [ ] :L: Support all `YulDSL` constructors.
- Yul CodeGen
  - CodeGen core:
    - [ ] 🚧 :M: Lazy semantics.
    - [ ] :S: Fn autoId (instead of using yulCatDigest.)
  - Object builder:
    - [ ] 🚧 :XL: dispatcher builder with full dispatcher calldata codec support.
    - [ ] :M: constructor support.
- PlantUML CodeGen
  - [ ] :L: Complete PlantUML codegen support.

## YulDSL Linear-SMC Frontend

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

## YOL Suite

- YOL stands for *Yet Original Language*.
- YOL suite is *For the New Pioneer* of EVM application development.

### yolc: the evil twin of solc

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

### attila: who wields the foundry, forges his path

- Test Pipeline: `attila test`
  - [ ] QuickCheck integration using Eval monad.
  - [ ] Foundry testing integration using stunt contract.
- Deployment Pipeline: `attila deploy`
  - [ ] Deploy the program (program is an unit of deployment.)
  - [ ] Etherscan verification pipeline.

### drwitch: who persuades the tyrant, shapes our history


### Software Distribution

- [ ] :M: Nix flake.

Future Ideas
------------

- Liquid Haskell integration
- YulDSL artifact.
- Effect system?
