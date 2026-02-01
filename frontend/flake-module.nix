{ ... }:
{
  perSystem = { pkgs, ... }:
    let
      frontend = import ./package.nix { inherit pkgs; };
    in
    {
      packages.imako-frontend = frontend;
      devShells.frontend = pkgs.mkShell {
        name = "imako-frontend";
        packages = with pkgs; [ nodejs ];
      };
    };
}
