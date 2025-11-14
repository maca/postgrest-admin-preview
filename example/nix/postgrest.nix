{ pkgs }:

let
  run-postgrest = pkgs.writeShellScriptBin "run-postgrest" ''
    echo "Starting PostgREST..."

    if [ ! -f "$PWD/postgrest.conf" ]; then
      echo "Error: postgrest.conf not found in current directory"
      exit 1
    fi

    # Remove existing socket if it exists
    rm -f /tmp/postgrest-example.sock

    # Set PGRST_DB_URI environment variable to override db-uri in config
    export PGRST_DB_URI="postgres:///?host=$PWD/database/pgdata&dbname=example"

    ${pkgs.postgrest}/bin/postgrest $PWD/postgrest.conf &
    POSTGREST_PID=$!

    cleanup() {
      echo "Stopping PostgREST..."
      if kill -0 "$POSTGREST_PID" 2>/dev/null; then
        kill "$POSTGREST_PID" 2>/dev/null
      fi
      rm -f /tmp/postgrest-example.sock
      exit
    }

    trap cleanup INT TERM

    echo "PostgREST started with PID: $POSTGREST_PID"
    echo "Unix socket: /tmp/postgrest-example.sock"
    wait $POSTGREST_PID
  '';

in
{
  scripts = [ run-postgrest ];
  buildInputs = [ pkgs.postgrest ];
}
