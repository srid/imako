{ inputs, ... }:
{
  imports = [
    (inputs.git-hooks + /flake-module.nix)
  ];
  perSystem = { config, self', pkgs, lib, ... }: {
    pre-commit.settings = {
      hooks = {
        nixpkgs-fmt.enable = true;
        dx-fmt = {
          enable = true;
          name = "dx fmt";
          entry = "${pkgs.dioxus-cli}/bin/dx fmt --all-code";
          types = [ "rust" ];
          language = "system";
          pass_filenames = false;
        };
      };
    };
  };
}
