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

TODOs
-----

## Framework Features

- YulDSL Core
  - ContractABI: Solidity-Contract-ABI-Compatible Types
    - Primitive Types:
      - [x] Simple Static value types: `ADDR`, `BOOL`, `INTx s n`.
      - [ ] :S: `SVALUE`, storage value.
      - [ ] :S: `type BYTESnn = BYTES_N n`, static bytes value type.
      - [ ] :L: `BYTES(l?) n?`, optionally-length-indexed dynamic bytes type.
      - [ ] :M: `STRING`, dynamic utf-8 string type.
      - [ ] :M: `ARRAY(l?) (True|False) n? a, DARR, SARR`, optionally-length-indexed dynamic or static array type.
    - Derivative Types:
      - Tuple Types
        - [x] :L: `:*`, n-ary product.
        - [ ] :S: `SOLO a`, demarcation solo tuple.
        - [ ] 🔴 :L: `(,..)`, n-tuple types for function specification.
      - Function Types:
        - [ ] :M: `SEL`, selector data type; selector creators.
        - [x] :S: `FUNC a b`, external function reference with storage tag and effect tag.
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
        - [ ] Catesian: `YulFork, ▵; YulExl, π₁; YulExr, π₂; YulDis, ε; YulDup, δ;`
      - Control Flow Primitives:
        - [x] `YulEmbed`, embedding constant.
        - [ ] :M: 🚧 `YulITE`, if-then-else.
        - [ ] :M: `YulJump`, internal function calls.
        - [ ] :M: `YulCall`, external function calls.
        - [ ] :M: `YulMap, YulFoldl`, control structure for lists.
      - Storage Primitives:
        - [ ] :M: `YulView`, for indexed or named position.
        - [ ] :M: `YulGet, YulPut` using `REF`, and remove `YulSet, YulSPut`.
  - YulObject
    - [ ] :S: Module documentation.
- Eval Monad:
  - [ ] :L: Support all `YulDSL` constructors.
- YulDSL Linear-SMC Frontend:
  - Multi-style functions:
    - [x] :S: composition of all styles using `(>.>)`.
    - [x] :S: `IfThenElse` typeclass, rebindable if-then-else syntax.
  - YulCat Combinators
    - [x] :S: `Num, MPOrd` instances for `YulNum`.
    - [x] :S: `IfThenElse` instance.
    - [x] :M: `vfn, ap'vfn`, value function declaration and application.
  - Yul Port Combinators
    - [x] :S: `Num, MPOrd` instances for `YulNum`.
    - [x] :S: `IfThenElse` instance.
    - [x] :M: `lfn, ap'lfn`, linearly-typed function declaration and application.
  - Prelude:
    - [ ] :L: Curation.
- CodeGen
  - Yul
    - [ ] 🚧 :M: Object builder, constructor, dispatcher.
    - [ ] 🚧 :M: Lazy semantics.
    - [ ] :S: Fn autoId.
    - [ ] :M: Variable over-copying optimization.
    - [ ] :XL: Full dispatcher calldata codec support.
  - PlantUML
    - [ ] :L: **FULL** PlantUML support.
- Program Builder
  - [ ] :M: Stunt contract generator.
  - [ ] :M: Upgradability.
  - [ ] :M: Factory pattern.
  - [ ] :M: External library.

## Development Environment

- Test Pipeline: `yolc test`
  - [ ] QuickCheck integration using Eval monad.
  - [ ] Foundry testing integration using stunt contract.
- Deployment Pipeline: `yolc deploy`
  - [ ] Deploy the program (program is an unit of deployment.)
  - [ ] Etherscan verification pipeline.

Future Ideas
------------

- Liquid Haskell integration
- YulDSL artifact.
- Effect system?
