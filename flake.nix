{ 
  description = "UHOI - A DSL for Bio-standards";
  inputs = { 
    # nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils, ... }@inputs: 
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        # Use GHC 9.12.2, pkgs.haskellPackages is GHC 9.8.4
        hs_pkgs = pkgs.haskell.packages.ghc9122; 
      in {
        packages.default = hs_pkgs.callCabal2nix "uhoi" self {};
      }
    );
}
