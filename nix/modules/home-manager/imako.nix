{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.imako;
in
{
  options.services.imako = {
    enable = mkEnableOption "Imako for Obsidian";

    package = mkOption {
      type = types.package;
      description = "The Imako package to use";
      example = literalExpression "inputs.imako.packages.\${pkgs.system}.imako";
    };

    vaultDir = mkOption {
      type = types.str;
      example = "\${config.home.homeDirectory}/notes";
      description = "Path to your Obsidian vault";
    };

    port = mkOption {
      type = types.port;
      default = 4009;
      description = "Port to run the web server on";
    };

    host = mkOption {
      type = types.str;
      default = "localhost";
      description = "Host to bind the web server to";
    };

    https = mkOption {
      type = types.bool;
      default = true;
      description = "Enable HTTPS (TLS certificates auto-generated in vault/.imako)";
    };
  };

  config = mkIf cfg.enable {
    # Linux systemd user service
    systemd.user.services.imako = mkIf pkgs.stdenv.isLinux {
      Unit = {
        Description = "Imako for Obsidian";
        After = [ "network.target" ];
      };

      Service = {
        Type = "simple";
        ExecStart = "${cfg.package}/bin/imako ${cfg.vaultDir} --port ${toString cfg.port} --host ${cfg.host}${optionalString (!cfg.https) " --no-https"}";
      };

      Install = {
        WantedBy = [ "default.target" ];
      };
    };

    # Darwin launchd service
    launchd.agents.imako = mkIf pkgs.stdenv.isDarwin {
      enable = true;
      config = {
        ProgramArguments = [
          "${cfg.package}/bin/imako"
          cfg.vaultDir
          "--port"
          (toString cfg.port)
          "--host"
          cfg.host
        ] ++ optionals (!cfg.https) [ "--no-https" ];
        RunAtLoad = true;
        KeepAlive = false;
        StandardOutPath = "${config.home.homeDirectory}/Library/Logs/imako.log";
        StandardErrorPath = "${config.home.homeDirectory}/Library/Logs/imako.err";
      };
    };
  };
}
