{ pkgs }:

let
  # Generate postgrest.conf based on environment
  postgrestConf = pkgs.writeText "postgrest.conf" ''
    # db-uri is set via PGRST_DB_URI environment variable in run-postgrest script
    db-uri = "postgres:///?host=database/pgdata&dbname=example"
    db-schemas = "public, bluebox"
    db-anon-role = "web_anon"
    jwt-secret = "DL+P8+muauKgOSqRKqIKMkjcUpLZ5ajXScgA965i/Bg="
    server-host = "127.0.0.1"
    server-port = 9080
  '';

  run-postgrest = pkgs.writeShellScriptBin "run-postgrest" ''
    echo "Starting PostgREST..."

    # Remove existing socket if it exists
    rm -f /tmp/postgrest-example.sock
    rm -f /tmp/pgadmn-socket.sock

    # Set PGRST_DB_URI environment variable to override db-uri in config
    export PGRST_DB_URI="postgres:///?host=$PWD/database/pgdata&dbname=example"

    ${pkgs.postgrest}/bin/postgrest ${postgrestConf} &
    POSTGREST_PID=$!

    cleanup() {
      echo "Stopping PostgREST..."
      if kill -0 "$POSTGREST_PID" 2>/dev/null; then
        kill "$POSTGREST_PID" 2>/dev/null
      fi
      rm -f /tmp/postgrest-example.sock
      rm -f /tmp/pgadmn-socket.sock
      exit
    }

    trap cleanup INT TERM

    echo "PostgREST started with PID: $POSTGREST_PID"
    wait $POSTGREST_PID
  '';

in
{
  scripts = [ run-postgrest ];
  buildInputs = [ pkgs.postgrest ];
}
