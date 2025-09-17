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
    commonmark-simple.url = "github:srid/commonmark-simple";
    commonmark-simple.flake = false;
    commonmark-wikilink.url = "github:srid/commonmark-wikilink";
    commonmark-wikilink.flake = false;

    htmx.url = "github:JonathanLorimer/htmx";
    htmx.flake = false;
    co-log-effectful.url = "github:eldritch-cookie/co-log-effectful";
    htmx-extensions.url = "github:juspay/htmx-extensions/sse-unload"; # https://github.com/bigskysoftware/htmx-extensions/pull/147
    htmx-extensions.flake = false;
    # https://github.com/bflyblue/servant-event-stream/pull/13
    servant-event-stream.url = "github:bflyblue/servant-event-stream";
    servant-event-stream.flake = false;
  };

  outputs = inputs:
    # This will import ./nix/modules/flake/*.nix
    # cf. https://nixos-unified.org/autowiring.html#flake-parts
    #
    # To write your own Nix, add or edit files in ./nix/modules/flake-parts/
    inputs.nixos-unified.lib.mkFlake
      { inherit inputs; root = ./.; };
}
