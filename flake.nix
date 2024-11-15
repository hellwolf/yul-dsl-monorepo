{
  description = "Nix flake for the YulDSL monorepo";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, flake-utils, ... }: (flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
      commonDevInputs = with pkgs; [
        jq
        shellcheck
      ];
      shellHook = ''
        # This makes binaries of this project available for testing, e.g. `yolc`
        export PATH=$PWD/hs-pkgs/yol-suite/bin/:$PATH
      '';
    in {
      devShells.default = pkgs.mkShell {
        buildInputs = with pkgs; commonDevInputs ++ [
          # local dev tooling
          nodePackages.nodemon
          # haskell tooling
          cabal-install
          haskell.compiler.ghc910
          haskell.packages.ghc98.hlint_3_8
          haskell.packages.ghc910.haskell-language-server
        ];
        inherit shellHook;
      };
    }));
  }
