{ pkgs }:
pkgs.buildNpmPackage {
  pname = "imako-frontend";
  version = "0.0.1";
  src = ./.;
  npmDepsHash = "sha256-HqE3Bvw3bggDpOPn4kimlQ0wZ0Ze+gHGS3DWnwzYTKM=";
  buildPhase = ''
    npm run build
  '';
  installPhase = ''
    cp -r dist $out
  '';
}
