{
  description = "Nix flake for the YulDSL monorepo";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    haskell-tooling.url = "github:hellwolf/haskell-tooling.nix";
  };

  outputs = { nixpkgs, haskell-tooling, flake-utils, ... }: (flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
      requiredInputs = haskell-tooling.lib.install pkgs ["ghc96+hls"];
      devInputs = with pkgs; [
        nodePackages.nodemon
        jq
        shellcheck
      ];
      shellHook = ''
        # This makes binaries of this project available for testing, e.g. `yolc`
        export PATH=$PWD/hs-pkgs/yol-suite/bin/:$PATH
      '';
    in {
      devShells.default = pkgs.mkShell {
        buildInputs = requiredInputs ++ devInputs;
        inherit shellHook;
      };
    }));
}
