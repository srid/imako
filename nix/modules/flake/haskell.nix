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
          (root + /imako)
          (root + /cabal.project)
          (root + /LICENSE)
          (root + /README.md)
        ];
      });

      # basePackages = pkgs.haskellPackages;

      packages = {
        unionmount.source = inputs.unionmount;
        commonmark-simple.source = inputs.commonmark-simple;

        htmx.source = inputs.htmx + /htmx;
        htmx-lucid.source = inputs.htmx + /htmx-lucid;
        htmx-servant.source = inputs.htmx + /htmx-servant;
      };

      # Add your package overrides here
      settings = {
        imako = {
          stan = true;
        };

        servant-event-stream = {
          broken = false;
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
