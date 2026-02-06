{ inputs, ... }:
{
  imports = [
    inputs.process-compose-flake.flakeModule
  ];

  perSystem = { self', pkgs, lib, ... }: {
    # Server-only config for local development
    process-compose."e2e-servers" = {
      settings = {
        processes = {
          backend = {
            command = "${lib.getExe self'.packages.imako} ./example";
            readiness_probe = {
              exec = {
                # Accept any response from backend (including 404)
                command = "${pkgs.curl}/bin/curl -s -o /dev/null -w '%{exitcode}' http://localhost:4009 || true";
              };
              initial_delay_seconds = 3;
              period_seconds = 1;
              failure_threshold = 30;
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
            # Start frontend after backend starts (not waits for healthy)
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
            command = "${lib.getExe self'.packages.imako} ./example";
            readiness_probe = {
              exec = {
                command = "${pkgs.curl}/bin/curl -s -o /dev/null -w '%{exitcode}' http://localhost:4009 || true";
              };
              initial_delay_seconds = 3;
              period_seconds = 1;
              failure_threshold = 30;
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
