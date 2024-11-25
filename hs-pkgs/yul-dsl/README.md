YulDSL - A DSL For Solidity/YUl
===============================

YulDSL provides an embedded domain-specific language (EDSL) called 'YulDSL' for transpiling Haskell code to Solidiy/Yul
code.

Motivation
----------

> They (the programming languages) also differ in physical appearance, and more important, in logical structure. The
> question arises, do the idiosyncracies reflect basic logical properties of the situations that are being catered for?
> Or are they accidents of history and personal background that may be obscuring fruitful developments?
>
> — "The Next 700 Programming Languages" by P. J. Landin.

* Many emerging ecosystems utilize specialized programming languages designed specifically for their needs. For example,
  the Solidity programming language was created for the Ethereum Virtual Machine (EVM) ecosystem.

* These new languages serve no purpose beyond programming for their specific ecosystems.

* Meanwhile, advancements in programming language theories, especially type theory and category theory, continue.

* Among these languages, a few have demonstrated the capability to embed a domain-specific language (EDSL) to solve a
  wide range of problems without having to build a new programming language toolchain.

* In an effort to provide a safe, purely functional, high-level programming language for the Ethereum smart contracts
  development ecosystem, the author embarked on a journey to create an EDSL in Haskell for such a purpose.

A DSL Based On Category Theory
========================

The signature design choice for YulDSL is to base itself on [category
theory](https://ncatlab.org/nlab/show/category+theory). In contrast to ones that base themselves on stack machines or
lambda calculi.

In simple terms, this means that all the "nouns" of this language are the types of values, and all the "verbs" in this
language describe the transition from one type of value to another. Hence, a "verb" always connects two "nouns."

Following the analogy, each "sentence" has one verb, and "sentences" can compose with each other by matching their
"nouns" types to form a long "paragraph," aka. a program.

The advantage of such a construction hinges on a few prior works that, by providing a specific (namely, mathematically
lawful) set of "verbs," you can convert from this language to a different domain. Notably, these are two known works of
this fashion:

1) ["Compiling to Categories" by Conal Elliott (2017)](http://conal.net/papers/compiling-to-categories/) shows how to
translate the cartesian closed category into simply typed lambda calculus.

2) ["Evaluating Linear Functions to Symmetric Monoidal Categories" by Jean-Philippe Bernardy and Arnaud Spiwack
(2021)](https://arxiv.org/abs/2103.06195) explores the correspondence between linear types supported in Haskell and
symmetric monoidal categories (SMC).

YulDSL/Haskell
===========

YulDSL currently supports SMC. Due to SMC's correspondence with linear types, "YulDSL/Haskell" utilizes the theory and
makes it a safe programming experience for Ethereum using linear types.

To learn more about the YulDSL/Haskell, read [the README of its package](../yul-dsl-linear-smc/README.md).

Portable YulDSL Artifact
========================

> [!Caution]
>
> This is still at the ideation phase.

'YulDSL' can be serialized into a portable artifact, thereby enabling various front-ends, such as a visual programming
interface or other YulDSL-supported programming languages, to produce and share 'YulDSL' as portable modules with each
other.
