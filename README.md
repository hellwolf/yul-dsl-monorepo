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

- Flexible & Solidity-Contract-ABI-Compatible Fundamental Types
  - Value Types:
    - [x] `ADDR`, `BOOL`, `INTx s n`.
  - Composite Types:
    - [-] `::>`, chaining tuple constructor.
    - [ ] `BYTES`, arbitrary bytes.
    - [ ] `[a]`, list.
    - [ ] `deriving TUPLE`, deriving tuple generically.
  - Function Types:
    - [ ] `SEL`, selector data type.
    - [ ] `CALL SEL ADDR`, Solidity-Contract-ABI-compatible selector for external function calls.
    - [ ] `FUN a b`, internal function reference ??
- YulCat:
  - [ ] `Category` instance for the base library, in order to use `(>>>)`.
  - [ ] Type-Level Function Permission Tags.
  - [ ] `YulMap, YulFoldl`, control structure for lists.
  - [ ] `YulCall`, external function calls. (external|delegated|static_call ??)
- Eval Monad:
  - [ ] Contract ABI Codec in Haskell.
  - [ ] Test all `YulCat` constructors.
- Multi-Style Functions:
  - `fn`, point-free categorical functions.
    - [ ] Composition of different styles.
  - `vfn`, value functions.
    - [ ] Num typeclass instances.
  - `lfn`, linearly-typed functions.
    - [ ] Selection of linear combinators.
- CodeGen
  - [ ] Contract ABI Codec in Yul.
  - [ ] Object constructor.

## Development Environment

- Stunt Contract Support:
  - [ ] Generator.
- QuickCheck integration:
  - [ ] Testing using Eval monad.
  - [ ] `yolc test` pipeline support.
- Foundry testing integration
  - [ ] Testing using stunt contract.
  - [ ] `yolc test` pipeline support.
- Foundry deployment integration
  - [ ] Deploy stunt contract.
  - [ ] Etherscan verification pipeline.

Future Ideas
------------

- Effect System?
