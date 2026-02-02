{ root, inputs, ... }:
{
  perSystem = { self', pkgs, ... }: {
    # Check that TypeScript types are up to date with Haskell types
    checks.types-ts-up-to-date = pkgs.runCommand "types-ts-up-to-date"
      {
        buildInputs = [ self'.packages.generate-types pkgs.gnused ];
      } ''
      # Generate fresh protocol types
      generate-types protocol | sed 's/^type /export type /; s/^interface /export interface /' > generated-protocol.ts

      # Compare with checked-in version
      if ! diff -u ${root}/frontend/src/types.ts generated-protocol.ts; then
        echo ""
        echo "ERROR: frontend/src/types.ts is out of date!"
        echo "Run 'just generate-types' to regenerate it."
        exit 1
      fi

      # Generate fresh AST types
      generate-types ast | sed 's/^type /export type /; s/^interface /export interface /' > generated-ast.ts

      # Compare with checked-in version
      if ! diff -u ${root}/frontend/src/components/markdown/types.ts generated-ast.ts; then
        echo ""
        echo "ERROR: frontend/src/components/markdown/types.ts is out of date!"
        echo "Run 'just generate-types' to regenerate it."
        exit 1
      fi

      echo "TypeScript types are up to date"
      touch $out
    '';
  };
}
