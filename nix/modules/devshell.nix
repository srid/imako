{ inputs, ... }:
{
  perSystem = { config, self', pkgs, lib, ... }: {
    devShells.default = pkgs.mkShell {
      name = "imako-shell";
      inputsFrom = [
        self'.devShells.rust
        config.pre-commit.devShell
      ];
      packages = with pkgs; [
        just
        nixd
        bacon
        dioxus-cli
      ];
    };
  };
}
