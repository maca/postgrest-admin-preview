{
  description = "PostgREST Admin Preview Example - PostgreSQL + PostgREST demo";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Import modules
        bluebox = import ./nix/bluebox.nix { inherit pkgs; };
        postgres = import ./nix/postgres.nix { inherit pkgs bluebox; };
        postgrest = env: import ./nix/postgrest.nix { inherit pkgs env; };
        staticAssets = import ./nix/static-assets.nix { inherit pkgs; };
        elmWatch = import ./nix/elm-watch.nix { inherit pkgs; };
        scripts = env: import ./nix/scripts.nix {
          inherit pkgs env;
          postgresql = postgres.postgresql;
        };

        mkDevShell = env:
          let
            postgrestEnv = postgrest env;
            scriptsEnv = scripts env;
          in
          pkgs.mkShell {
            buildInputs = with pkgs; [
              curl
              elmPackages.elm
              nodejs
            ]
            ++ postgres.buildInputs
            ++ postgres.scripts
            ++ postgrestEnv.buildInputs
            ++ postgrestEnv.scripts
            ++ scriptsEnv.scripts;

            shellHook = ''
              export PGDATA=$PWD/database/pgdata
              export PGHOST=$PWD/database/pgdata
              export PGDATABASE=example

              ${if env == "develop" then ''
              # Add elm-watch from nix to PATH
              export PATH="${elmWatch.nodeModules}/node_modules/.bin:$PATH"
              '' else ''
              # Compile Elm to production JS
              echo "Compiling Elm application..."
              mkdir -p "$PWD/static"
              elm make src/Main.elm --optimize --output=static/main.js
              echo "Elm compilation complete: static/main.js"
              ''}

              # Setup static assets
              mkdir -p "$PWD/static"
              cp -f "${staticAssets.redoc}/redoc.standalone.js" "$PWD/static/redoc.standalone.js"
              cp -f "${staticAssets.icono}/icono.min.css" "$PWD/static/icono.min.css"
              cp -f "${staticAssets.milligram}/milligram.min.css" "$PWD/static/milligram.min.css"

              echo "PostgREST Admin Preview - Example Environment (${env})"
              echo "PostgreSQL data dir: $PGDATA"
              echo ""
              echo ""
              echo "Available commands:"
              echo "  setup           - Initialize database and load dump (run this first)"
              echo "  run [port]      - Start all services, default port 9000"
              echo "  run-postgres    - Start PostgreSQL service only"
              echo "  run-postgrest   - Start PostgREST service only"
              echo "  load-dump       - Reload schema into database"
              echo "  database        - Open database shell (psql)"
              echo "  stop            - Stop all services"
              echo "  clean           - Remove database and start fresh"
            '';
          };
      in
      {
        # Export packages
        packages = {
          inherit (staticAssets) redoc icono milligram;
        };

        devShells.default = mkDevShell "develop";
        devShells.production = mkDevShell "production";
      });
}
