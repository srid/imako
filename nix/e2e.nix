{ inputs, ... }:
{
  imports = [
    inputs.process-compose-flake.flakeModule
  ];

  perSystem = { self', pkgs, lib, ... }: {
    # Local development: ghcid backend + Vite frontend with hot-reload
    process-compose."dev" = {
      settings = {
        processes = {
          backend = {
            # Use ghcid for hot-reload development (NOTEBOOK env var for vault path)
            command = "${pkgs.ghcid}/bin/ghcid --outputfile=ghcid.txt -T Main.main -c 'cabal repl --enable-multi-repl imako:exe:imako' --setup \":set args $NOTEBOOK\"";
            readiness_probe = {
              exec = {
                command = "${pkgs.netcat}/bin/nc -z localhost 4009";
              };
              initial_delay_seconds = 10; # ghcid takes longer to start
              period_seconds = 2;
              failure_threshold = 60;
            };
          };

          frontend = {
            command = "cd frontend && ${pkgs.nodejs}/bin/npm run dev";
            readiness_probe = {
              http_get = {
                host = "localhost";
                port = 5173;
                path = "/";
              };
              initial_delay_seconds = 2;
              period_seconds = 1;
              failure_threshold = 30;
            };
            depends_on.backend.condition = "process_healthy";
          };
        };
      };
    };

    # Full E2E test runner for CI - starts server, runs tests, exits with test result
    # Uses unique port to avoid conflicts with dev servers (4009)
    # The Nix-packaged imako binary bundles the frontend (via IMAKO_FRONTEND_PATH)
    process-compose."e2e" =
      let
        port = 4019;
        imako = lib.getExe' self'.packages.imako-with-frontend "imako";
      in
      {
        # Disable TUI for CI
        cli.environment.PC_DISABLE_TUI = true;

        settings = {
          processes = {
            imako = {
              # The Nix package bundles the frontend - no separate frontend process needed
              command = "set -x; ${imako} --port ${toString port} ./example";
              readiness_probe = {
                http_get = {
                  host = "localhost";
                  inherit port;
                  path = "/";
                };
              };
            };

            test-runner = {
              command = "cd tests && E2E_BASE_URL=http://localhost:${toString port} npm run e2e";
              depends_on.imako.condition = "process_healthy";
              # Exit process-compose when tests complete
              availability.exit_on_end = true;
            };
          };
        };
      };
  };
}
