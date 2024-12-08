Yol-Suite: A Collection of YulDSL Programs for the New Pioneer of Ethereum Smart Contracts Development
======================================================================================================

> [!IMPORTANT]
>
> - YOL stands for *Yet Original Language*.
>
> - YOL suite is [*for the new pioneer*](https://en.wikipedia.org/wiki/For_the_New_Intellectual).

Yolc
====

> [!NOTE]
>
> Yolc is the command line app to work with YulDSL/Haskell projects.

Project Builder
---------------

### Features

* Cabal build system integration.
* A manifest haskell module that describes:
  * Contract size strategy:
    * ✅ No logic split (EIP-170 contract code size limit applicable),
    * [ ] Manual logic split & dispatching,
    * [ ] Auto logic split.
  * Deployment types:
    * ✅ Singleton contract,
    * [ ] Factory contract,
    * [ ] Shared library.
  * Upgradability:
    * ✅ Immutable contract,
    * [ ] Singleton upgradability,
    * [ ] Beacon Upgradability.
* Contract verification support
  - [ ] 🚧 Stunt contract generator.

CLI
---

`yolc [options] yol_module_spec...`

- Input modes:
  - `symbol   :: FnCat a b`, fnMode
  - `object   :: YulObject`, objectMode
  - `manifest :: Manifest`, projectMode
- Codegen mode:
  - Show
  - Yul
  - Diagrams

Attila
======

TODO.

DrWitch
=======

TODO.
