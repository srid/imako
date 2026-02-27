{ inputs, ... }:
{
  imports = [
    inputs.process-compose-flake.flakeModule
  ];

  perSystem = { self', pkgs, lib, ... }: {
    # Local development: dx serve with hot-reload
    process-compose."dev" = {
      # Disable web UI to avoid port 8080 conflicts
      cli.options.no-server = true;
      settings = {
        processes = {
          imako = {
            command = "IMAKO_VAULT_PATH=./example dx serve --port 5173";
            readiness_probe = {
              http_get = {
                host = "localhost";
                port = 5173;
                path = "/";
              };
            };
          };
        };
      };
    };
  };
}
