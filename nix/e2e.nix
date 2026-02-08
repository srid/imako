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
              http_get = {
                host = "localhost";
                port = 4009;
                path = "/health";
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
        # Disable web UI to avoid port 8080 conflicts
        cli.options.no-server = true;

        settings = {
          processes = {
            imako = {
              # The Nix package bundles the frontend - no separate frontend process needed
              command = "set -x; ${imako} --port ${toString port} ./example";
              readiness_probe = {
                http_get = {
                  host = "localhost";
                  inherit port;
                  path = "/health";
                };
              };
            };

            test-runner = {
              command = "cd tests && npm ci && E2E_BASE_URL=http://localhost:${toString port} npx playwright test";
              depends_on.imako.condition = "process_healthy";
              # Exit process-compose when tests complete
              availability.exit_on_end = true;
            };
          };
        };
      };

    # E2E server only - for developing tests interactively
    # Run `just e2e-server` then `just e2e-test` in another terminal
    process-compose."e2e-server" =
      let
        port = 4019;
        imako = lib.getExe' self'.packages.imako-with-frontend "imako";
      in
      {
        settings = {
          processes = {
            imako = {
              command = "set -x; ${imako} --port ${toString port} ./example";
              readiness_probe = {
                http_get = {
                  host = "localhost";
                  inherit port;
                  path = "/health";
                };
              };
            };
          };
        };
      };
  };
}
