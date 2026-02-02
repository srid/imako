{ pkgs }:
pkgs.buildNpmPackage {
  pname = "imako-frontend";
  version = "0.0.1";
  src = ./.;
  npmDepsHash = "sha256-Oa0/3SXRBFCN95vpqfJjeIu9NxQJ7Hg4nle+XfyUdx8=";
  buildPhase = ''
    npm run build
  '';
  installPhase = ''
    cp -r dist $out
  '';
}
