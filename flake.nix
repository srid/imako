{
  description = "Imako — journaling and planning for your Obsidian notebook";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";
    systems.url = "github:nix-systems/default";

    rust-flake.url = "github:juspay/rust-flake";
    rust-flake.inputs.nixpkgs.follows = "nixpkgs";

    process-compose-flake.url = "github:Platonic-Systems/process-compose-flake";

    git-hooks.url = "github:cachix/git-hooks.nix";
    git-hooks.flake = false;
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;
      imports = [
        inputs.rust-flake.flakeModules.default
        inputs.rust-flake.flakeModules.nixpkgs
        inputs.process-compose-flake.flakeModule
        (inputs.git-hooks + /flake-module.nix)
        ./nix/home-manager/flake-module.nix
        ./nix/e2e.nix
      ];

      perSystem = { config, self', pkgs, lib, ... }: {
        rust-project = {
          # Crane build configuration for the imako crate
          crates."imako" = {
            path = ./.;
            crane.args = {
              buildInputs = lib.optionals pkgs.stdenv.isDarwin (
                with pkgs; [
                  apple-sdk_15
                  libiconv
                ]
              );
              nativeBuildInputs = with pkgs; [
                pkg-config
                tailwindcss_4
                dioxus-cli
              ];
            };
          };
          # Source filtering for Nix builds
          src = lib.cleanSourceWith {
            src = inputs.self;
            filter = path: type:
              (lib.hasSuffix ".html" path) ||
              (lib.hasSuffix ".css" path) ||
              (lib.hasInfix "/assets/" path) ||
              (lib.hasInfix "/crates/" path) ||
              (config.rust-project.crane-lib.filterCargoSources path type)
            ;
          };
        };

        packages.default = self'.packages.imako;

        # Pre-commit hooks
        pre-commit.settings.hooks = {
          nixpkgs-fmt.enable = true;
          rustfmt.enable = true;
        };

        # Development shell
        devShells.default = pkgs.mkShell {
          name = "imako";
          inputsFrom = [
            self'.devShells.rust
            config.pre-commit.devShell
          ];
          packages = with pkgs; [
            just
            dioxus-cli
            tailwindcss_4
            # For e2e tests
            nodejs
          ];
        };
      };
    };
}
