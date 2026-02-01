{
  description = "Nix template for Haskell projects";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";
    nixos-unified.url = "github:srid/nixos-unified";
    haskell-flake.url = "github:srid/haskell-flake";
    fourmolu-nix.url = "github:jedimahdi/fourmolu-nix";

    git-hooks.url = "github:cachix/git-hooks.nix";
    git-hooks.flake = false;

    unionmount.url = "github:srid/unionmount";
    unionmount.flake = false;
    lvar.url = "github:srid/lvar";
    lvar.flake = false;
    commonmark-simple.url = "github:srid/commonmark-simple";
    commonmark-simple.flake = false;
    commonmark-wikilink.url = "github:srid/commonmark-wikilink";
    commonmark-wikilink.flake = false;

    aeson-typescript.url = "github:codedownio/aeson-typescript";
    aeson-typescript.flake = false;

    warp-tls-simple.url = "github:srid/warp-tls-simple";
    warp-tls-simple.flake = false;
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        # Auto-imported modules from nixos-unified
        (inputs.nixos-unified.lib.mkFlake { inherit inputs; root = ./.; }).flakeModules.default
        # Domain-encapsulated module for generate-types
        ./packages/generate-types/flake-module.nix
      ];
    };
}


