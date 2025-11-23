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

      checks.${system} = {
        imako-home-manager-test = pkgs.testers.runNixOSTest {
          name = "imako-home-manager";

          nodes.machine = { ... }: {
            imports = [ home-manager.nixosModules.home-manager ];

            users.users.testuser = {
              isNormalUser = true;
              uid = 1000;
            };

            # Enable user services
            systemd.services."user@1000" = {
              overrideStrategy = "asDropin";
            };

            # Ensure runtime directory exists
            systemd.tmpfiles.rules = [
              "d /run/user/1000 0755 testuser testuser -"
            ];

            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              users.testuser = {
                imports = [ imako.homeManagerModules.imako ];

                services.imako = {
                  enable = true;
                  package = imako.packages.${system}.default;
                  vaultDir = "/home/testuser/notes";
                  port = 4009;
                  host = "0.0.0.0";
                };

                home = {
                  username = "testuser";
                  homeDirectory = "/home/testuser";
                  stateVersion = "24.05";
                };
              };
            };

            # Minimal VM configuration
            system.stateVersion = "24.05";
          };

          testScript = ''
            import time

            machine.start()
            machine.wait_for_unit("multi-user.target")

            # Create vault directory with a test note
            machine.succeed("mkdir -p /home/testuser/notes")
            machine.succeed("chown -R testuser:users /home/testuser/notes")
            machine.succeed("echo '# Test Note' > /home/testuser/notes/test.md")
            machine.succeed("chown testuser:users /home/testuser/notes/test.md")

            # Enable lingering for testuser to allow user services to run
            machine.succeed("loginctl enable-linger testuser")

            # Start a proper user session
            machine.succeed("machinectl shell testuser@ /bin/true")

            # Use systemctl --user directly with proper environment
            machine.succeed("sudo -u testuser XDG_RUNTIME_DIR=/run/user/1000 systemctl --user daemon-reload")
            machine.succeed("sudo -u testuser XDG_RUNTIME_DIR=/run/user/1000 systemctl --user start imako.service")

            # Wait for the service to start
            time.sleep(10)

            # Check if the service is running
            machine.succeed("sudo -u testuser XDG_RUNTIME_DIR=/run/user/1000 systemctl --user is-active imako.service")

            # Check if the service is listening on port 4009
            machine.wait_for_open_port(4009)

            # Test HTTP response
            machine.succeed("curl -k -f https://localhost:4009")

            print("✅ Imako home-manager service is running and responding to HTTP requests")
          '';
        };
      };
    };
}
