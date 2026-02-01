{ root, inputs, ... }:
{
  perSystem = { self', pkgs, ... }:
    let
      frontend = import (root + /frontend/package.nix) { inherit pkgs; };
    in
    {
      packages.imako-frontend = frontend;
    };
}
