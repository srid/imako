{ root, ... }:
{
  perSystem = { pkgs, ... }:
    let
      frontend = import ./package.nix { inherit pkgs; };
    in
    {
      packages.imako-frontend = frontend;
    };
}
