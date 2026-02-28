{ inputs, ... }:
{
  imports = [
    inputs.rust-flake.flakeModules.default
    inputs.rust-flake.flakeModules.nixpkgs
  ];
  perSystem = { config, self', pkgs, lib, ... }: {
    rust-project.crates."imako" = {
      path = ./../..;
    };
    packages.default = self'.packages.imako;
  };
}
