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
                command = "${pkgs.curl}/bin/curl -s -o /dev/null -w '%{exitcode}' http://localhost:4009";
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
            depends_on.backend.condition = "process_started";
          };
        };
      };
    };

    # Full E2E test runner for CI - starts servers, runs tests, exits with test result
    process-compose."e2e" = {
      # Disable TUI for CI
      cli.environment.PC_DISABLE_TUI = true;

      settings = {
        processes = {
          backend = {
            command = "set -x; ${lib.getExe self'.packages.imako} ./example";
            readiness_probe = {
              exec = {
                command = "${pkgs.curl}/bin/curl -s -o /dev/null -w '%{exitcode}' http://localhost:4009";
              };
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
            };
            depends_on.backend.condition = "process_started";
          };

          test-runner = {
            command = "cd tests && npm run e2e";
            depends_on = {
              backend.condition = "process_healthy";
              frontend.condition = "process_healthy";
            };
            # Exit process-compose when tests complete
            availability.exit_on_end = true;
          };
        };
      };
    };
  };
}
