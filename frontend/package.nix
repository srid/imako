{ pkgs }:
pkgs.buildNpmPackage {
  pname = "imako-frontend";
  version = "0.0.1";
  src = ./.;
  npmDepsHash = "sha256-ph/eJZ0911L/gaxO6+RJ4pW8CHQr1Xb/hT7XinuR5Ho=";
  buildPhase = ''
    npm run build
  '';
  installPhase = ''
    cp -r dist $out
  '';
}
