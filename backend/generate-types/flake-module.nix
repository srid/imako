{ root, inputs, ... }:
{
  perSystem = { self', pkgs, ... }:
    let
      # Shared script for generating TypeScript types
      # Used by: `nix run .#generate-types-to` and CI check
      generateTypesScript = pkgs.writeShellScriptBin "generate-types-to" ''
        set -euo pipefail
        OUTPUT_DIR="''${1:-.}"
        GEN_CMD="''${2:-generate-types}"

        TYPES_FILE="$OUTPUT_DIR/frontend/src/types.ts"
        AST_FILE="$OUTPUT_DIR/frontend/src/components/markdown/types.ts"

        # Protocol types with header (Inline import needed for Task.description)
        {
          echo '// AUTO-GENERATED - DO NOT EDIT. Regenerate with: just generate-types'
          echo 'import { Inline } from "@/components/markdown/types";'
          echo ""
          $GEN_CMD protocol | ${pkgs.gnused}/bin/sed 's/^type /export type /; s/^interface /export interface /'
        } > "$TYPES_FILE"

        # AST types
        $GEN_CMD ast | ${pkgs.gnused}/bin/sed 's/^type /export type /; s/^interface /export interface /' > "$AST_FILE"

        echo "Generated $TYPES_FILE"
        echo "Generated $AST_FILE"
      '';
    in
    {
      # Expose script as app for justfile: `nix run .#generate-types-to`
      apps.generate-types-to = {
        type = "app";
        program = "${generateTypesScript}/bin/generate-types-to";
      };

      # Check that TypeScript types are up to date with Haskell types
      checks.types-ts-up-to-date = pkgs.runCommand "types-ts-up-to-date"
        {
          buildInputs = [ self'.packages.generate-types generateTypesScript ];
        } ''
        mkdir -p frontend/src/components/markdown
        generate-types-to . generate-types

        if ! diff -u ${root}/frontend/src/types.ts frontend/src/types.ts; then
          echo ""
          echo "ERROR: frontend/src/types.ts is out of date!"
          echo "Run 'just generate-types' to regenerate it."
          exit 1
        fi

        if ! diff -u ${root}/frontend/src/components/markdown/types.ts frontend/src/components/markdown/types.ts; then
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
