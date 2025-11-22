{
  description = "Example Home Manager configuration with Imako";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    imako.url = "github:srid/imako";
  };

  outputs = { self, nixpkgs, home-manager, imako }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      homeConfigurations.example = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [
          # Import the Imako home-manager module
          imako.homeManagerModules.imako

          # Example Imako configuration
          {
            home = {
              username = "testuser";
              homeDirectory = "/home/testuser";
              stateVersion = "24.05";
            };

            services.imako = {
              enable = true;
              package = imako.packages.${system}.default;
              vaultDir = "/home/testuser/notes";
              port = 4009;
              host = "localhost";
            };
          }
        ];
      };
    };
}
