{ pkgs }:
pkgs.buildNpmPackage {
  pname = "imako-frontend";
  version = "0.0.1";
  src = ./.;
  npmDepsHash = "sha256-C92uOtV3WeTPLNv4Po/LvrNlqv0l3VYJLvYh949Afkg=";
  buildPhase = ''
    npm run build
  '';
  installPhase = ''
    cp -r dist $out
  '';
}
