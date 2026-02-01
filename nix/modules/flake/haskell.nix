{ root, inputs, ... }:
{
  imports = [
    inputs.haskell-flake.flakeModule
    (inputs.warp-tls-simple + /flake-module.nix)
  ];
  perSystem = { self', lib, pkgs, ... }: {
    haskellProjects.default = {
      projectFlakeName = "imako-monorepo";
      # To avoid unnecessary rebuilds, we filter projectRoot:
      # https://community.flake.parts/haskell-flake/local#rebuild
      projectRoot = builtins.toString (lib.fileset.toSource {
        inherit root;
        fileset = lib.fileset.unions [
          (root + /packages)
          (root + /cabal.project)
          (root + /LICENSE)
          (root + /README.md)
        ];
      });

      packages = {
        unionmount.source = inputs.unionmount;
        lvar.source = inputs.lvar;
        commonmark-simple.source = inputs.commonmark-simple;
        commonmark-wikilink.source = inputs.commonmark-wikilink;
        aeson-typescript.source = inputs.aeson-typescript;
      };

      settings = {
        unionmount.jailbreak = true;
        aeson-typescript.check = false; # Tests require tsc
      };

      # Development shell configuration
      devShell = {
        hlsCheck.enable = false;
      };

      # What should haskell-flake add to flake outputs?
      autoWire = [ "packages" "apps" "checks" ]; # Wire all but the devShell
    };

    # Wrapped imako package with bundled frontend
    packages.imako-with-frontend = pkgs.symlinkJoin {
      name = "imako-with-frontend";
      paths = [ self'.packages.imako ];
      buildInputs = [ pkgs.makeWrapper ];
      postBuild = ''
        # Wrap the imako binary to include frontend/dist
        wrapProgram $out/bin/imako \
          --set IMAKO_FRONTEND_PATH ${self'.packages.imako-frontend}
      '';
    };

    # Default package & app.
    packages.default = self'.packages.imako-with-frontend;
    apps.default = self'.apps.imako;

    # Check that frontend/src/types.ts is up to date
    checks.types-ts-up-to-date = pkgs.runCommand "types-ts-up-to-date"
      {
        buildInputs = [ self'.packages.imako pkgs.gnused ];
      } ''
      # Generate fresh types
      generate-types | sed 's/^type /export type /; s/^interface /export interface /' > generated.ts

      # Compare with checked-in version
      if ! diff -u ${root}/frontend/src/types.ts generated.ts; then
        echo ""
        echo "ERROR: frontend/src/types.ts is out of date!"
        echo "Run 'just generate-types' to regenerate it."
        exit 1
      fi

      echo "types.ts is up to date"
      touch $out
    '';
  };
}
