{ inputs, ... }:
{
  imports = [
    inputs.rust-flake.flakeModules.default
    inputs.rust-flake.flakeModules.nixpkgs
  ];
  perSystem = { config, self', pkgs, lib, ... }: {
    rust-project = {
      crates."imako" = {
        path = ./../..;
        crane.args = {
          nativeBuildInputs = with pkgs; [
            pkg-config
            makeWrapper
            tailwindcss
            dioxus-cli
            wasm-bindgen-cli
          ];
        };
        crane.extraBuildArgs = {
          # Build with the server feature for the server binary.
          cargoExtraArgs = "-p imako --features server";
        };
      };

      # Include non-Rust sources needed by dx build (Dioxus.toml, CSS, etc.)
      src = lib.cleanSourceWith {
        src = inputs.self;
        filter = path: type:
          (lib.hasSuffix "Dioxus.toml" path) ||
          (lib.hasSuffix "tailwind.config.js" path) ||
          (lib.hasSuffix "input.css" path) ||
          (lib.hasInfix "/public/" path) ||
          (lib.hasInfix "/assets/" path) ||
          (lib.hasInfix "/example/" path) ||
          (config.rust-project.crane-lib.filterCargoSources path type)
        ;
      };
    };

    # Override the default package to also build the WASM client assets
    # and bundle them alongside the server binary.
    packages.default = self'.packages.imako.overrideAttrs (oa: {
      nativeBuildInputs = (oa.nativeBuildInputs or [ ]) ++ (with pkgs; [
        dioxus-cli
        tailwindcss
        wasm-bindgen-cli
      ]);

      postBuild =
        (oa.postBuild or "") + ''
          # Build Tailwind CSS
          tailwindcss -i input.css -o public/tailwind.css --minify

          # Build the WASM client using dx
          dx build --platform web
        '';

      installPhase =
        (oa.installPhase or "") + ''
          # Copy the WASM assets (public/) next to the server binary
          if [ -d target/dx/imako/debug/web/public ]; then
            cp -r target/dx/imako/debug/web/public $out/bin/public
          elif [ -d target/dx/imako/release/web/public ]; then
            cp -r target/dx/imako/release/web/public $out/bin/public
          fi
        '';
    });
  };
}
