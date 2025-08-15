{ root, inputs, ... }:
{
  imports = [
    inputs.haskell-flake.flakeModule
  ];
  perSystem = { self', lib, ... }: {
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

      settings = {
        imako = {
          stan = true;
        };
      };

      # Development shell configuration
      devShell = {
        hlsCheck.enable = false;
      };

      # What should haskell-flake add to flake outputs?
      autoWire = [ "packages" "apps" "checks" ]; # Wire all but the devShell
    };

    # Default package & app.
    packages.default = self'.packages.imako;
    apps.default = self'.apps.imako;
  };
}
