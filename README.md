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
    - Value Types:
      - [-] `ADDR`, `BOOL`, `INTx s n`.
    - Composite Types:
      - [-] :M: `::>`, chaining tuple constructor.
      - [ ] :L: `BYTES`, arbitrary bytes.
      - [ ] :L: `[a]`, list.
      - [ ] :XL: `TUPLE`, tuple type.
      - [ ] :XL: `deriving TUPLE`, deriving tuple generically.
    - Function Types:
      - [ ] :S: `FUNC a b`, external function reference.
      - [-] :M: `SEL`, selector data type; selector creators.
    - [ ] :S: Cleanup inline-REPL docs.
  - YulCat
    - [x] `(>.>)` operator for the `YulDSL` morphism left-to-right composition.
    - [ ] :M: Type-Level Function Permission Tags.
    - [ ] :M: `YulMap, YulFoldl`, control structure for lists.
  - YulObject
    - [ ] :S: Module documentation.
- Eval Monad:
  - [ ] :L: Support all `YulDSL` constructors.
  - [ ] :XXL: **FULL** `ContractABI` Codec in Haskell.
- YulDSL Linear-SMC Frontend:
  - YulCat Combinators
    - [-] 🚧 :S: Num typeclass instances for `YulNum n => YulCat m n`.
  - Yul Port Combinators
  - Multi-style functions:
    - [x] `lfn`, linearly-typed functions.
    - [ ] 🚧 :L: `vfn`, value functions.
    - [ ] 🚧 :S: composition of all styles.
  - [ ] :L: Curating a prelude of linear smc combinators.
- CodeGen
  - Yul
    - [ ] :M: Object constructor.
    - [ ] :XXL: **FULL** `ContractABI` Codec in Yul.
  - PlantUML
    - [ ] :L: **FULL** PlantUML support.

## Development Environment

- QuickCheck integration
  - [ ] Testing using Eval monad.
  - [ ] `yolc test` pipeline support.
- Foundry testing integration
  - [ ] Testing using stunt contract.
  - [ ] `yolc test` pipeline support.
- Stunt Contract Support
  - [ ] Generator.
- Foundry deployment integration
  - [ ] `yolc deploy` pipeline.
  - [ ] Deploy stunt contract.
  - [ ] Etherscan verification pipeline.

Future Ideas
------------

- Effect System?
