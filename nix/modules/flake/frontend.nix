{ root, inputs, ... }:
{
  perSystem = { pkgs, ... }:
    let
      frontend = pkgs.buildNpmPackage {
        pname = "imako-frontend";
        version = "0.0.1";
        src = root + /frontend;
        npmDepsHash = "sha256-B76SQh8ywqYFERPXIIbeLMgaPau64+7RqY16E8O/OeY=";
        buildPhase = ''
          npm run build
        '';
        installPhase = ''
          cp -r dist $out
        '';
      };
    in
    {
      packages.imako-frontend = frontend;
    };
}
