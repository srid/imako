{ inputs, ... }:
{
  imports = [
    inputs.process-compose-flake.flakeModule
  ];

  perSystem = { self', pkgs, lib, ... }: {
    # Full E2E test runner — starts server, runs tests, exits with test result.
    # Usage: `nix run .#e2e` or `just e2e`
    process-compose."e2e" =
      let
        port = 6006;
        imako = lib.getExe self'.packages.default;
      in
      {
        cli.environment.PC_DISABLE_TUI = true;
        cli.options.no-server = true;

        settings = {
          processes = {
            imako = {
              command = "VAULT_PATH=./example PORT=${toString port} ${imako}";
              readiness_probe = {
                period_seconds = 1;
                http_get = {
                  host = "localhost";
                  inherit port;
                  path = "/";
                };
              };
            };

            test-runner = {
              command = "cd tests && npm ci && npx playwright install --with-deps chromium && npx playwright test";
              depends_on.imako.condition = "process_healthy";
              # Exit process-compose when tests complete
              availability.exit_on_end = true;
            };
          };
        };
      };

    # E2E server only — for developing tests interactively.
    # Run `just e2e-server` then `just test-e2e` in another terminal.
    process-compose."e2e-server" =
      let
        port = 6006;
        imako = lib.getExe self'.packages.default;
      in
      {
        settings = {
          processes = {
            imako = {
              command = "VAULT_PATH=./example PORT=${toString port} ${imako}";
              readiness_probe = {
                http_get = {
                  host = "localhost";
                  inherit port;
                  path = "/";
                };
              };
            };
          };
        };
      };
  };
}
