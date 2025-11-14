{ pkgs }:

let
  # Build npm dependencies from package.json and package-lock.json
  nodeModules = pkgs.buildNpmPackage {
    pname = "postgrest-admin-example-deps";
    version = "1.0.0";

    src = ../.;

    npmDepsHash = "sha256-Mhz9z69MFHw0EmWnp10aTrxD2w4MHR10lQWe/dJxZAI=";

    # We only want to install dependencies, not build anything
    dontNpmBuild = true;

    installPhase = ''
      mkdir -p $out
      cp -r node_modules $out/
    '';
  };

in
{
  inherit nodeModules;

  # Provide elm-watch binary directly
  elmWatch = "${nodeModules}/node_modules/.bin/elm-watch";
}
