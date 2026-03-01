{ inputs, ... }:
{
  imports = [
    (inputs.git-hooks + /flake-module.nix)
  ];
  perSystem = { config, self', pkgs, lib, ... }: {
    pre-commit.settings = {
      hooks = {
        nixpkgs-fmt.enable = true;
        # dx fmt first (rsx!), then rustfmt (regular Rust from rustfmt.toml)
        dx-fmt = {
          enable = true;
          name = "dx fmt";
          entry = "${pkgs.dioxus-cli}/bin/dx fmt";
          types = [ "rust" ];
          language = "system";
          pass_filenames = false;
        };
        rustfmt.enable = true;
      };
    };
  };
}
