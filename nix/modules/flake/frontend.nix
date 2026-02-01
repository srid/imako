{ root, inputs, ... }:
{
  perSystem = { self', pkgs, ... }:
    let
      frontend = import (root + /frontend/package.nix) { inherit pkgs; };
    in
    {
      packages.imako-frontend = frontend;

      # Check that frontend/src/types.ts is up to date with Haskell types
      checks.types-ts-up-to-date = pkgs.runCommand "types-ts-up-to-date"
        {
          buildInputs = [ self'.packages.generate-types pkgs.gnused ];
        } ''
        # Generate fresh types
        generate-types | sed 's/^type /export type /; s/^interface /export interface /' > generated.ts

        # Compare with checked-in version
        if ! diff -u ${root}/frontend/src/types.ts generated.ts; then
          echo ""
          echo "ERROR: frontend/src/types.ts is out of date!"
          echo "Run 'just generate-types' to regenerate it."
          exit 1
        fi

        echo "types.ts is up to date"
        touch $out
      '';
    };
}
