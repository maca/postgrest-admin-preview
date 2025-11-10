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
      in
      let
        postgresql = pkgs.postgresql_17.withPackages (ps: [
          ps.pgjwt
        ]);

        # Fetch Pagila sample database SQL files from GitHub
        pagilaSchema = pkgs.fetchurl {
          url = "https://raw.githubusercontent.com/devrimgunduz/pagila/master/pagila-schema.sql";
          sha256 = "sha256-jONY5MgBQIe4UpZpSgiTiHvXpBkOPOQH8nIbhrmOVwc=";
        };

        pagilaData = pkgs.fetchurl {
          url = "https://raw.githubusercontent.com/devrimgunduz/pagila/master/pagila-data.sql";
          sha256 = "sha256-iAWA+yzU2qqZ8pDO0mSYjN1lezFYvmPNKBRm95b22/I=";
        };

        # Helper script to load all SQL files
        load-sql = pkgs.writeShellScriptBin "load-sql" ''
          export PGDATA=$PWD/database/pgdata
          export PGHOST=$PWD/database/pgdata
          export PGDATABASE=example

          echo "Loading Pagila schema from ${pagilaSchema}..."
          ${postgresql}/bin/psql --host="$PGHOST" -d example -f "${pagilaSchema}"

          echo "Loading Pagila data from ${pagilaData}..."
          ${postgresql}/bin/psql --host="$PGHOST" -d example -f "${pagilaData}"

          echo "Loading database schema from database/schema.sql..."
          ${postgresql}/bin/psql --host="$PGHOST" -d example -f "$PWD/database/schema.sql"

          echo "Loading sample data from database/data.sql..."
          ${postgresql}/bin/psql --host="$PGHOST" -d example -f "$PWD/database/data.sql"

          echo "Loading permissions from database/permissions.sql..."
          ${postgresql}/bin/psql --host="$PGHOST" -d example -f "$PWD/database/permissions.sql"

          echo "Database loaded successfully (Pagila + custom schema + permissions)"
        '';

        setup = pkgs.writeShellScriptBin "setup" ''
          echo "Setting up database..."

          # Set up PostgreSQL environment variables
          export PGDATA=$PWD/database/pgdata
          export PGHOST=$PWD/database/pgdata
          export PGDATABASE=example

          # Initialize PostgreSQL if it doesn't exist
          if [ ! -f "$PGDATA/PG_VERSION" ]; then
            echo "Initializing PostgreSQL database..."
            mkdir -p "$PGDATA"
            ${postgresql}/bin/initdb \
              -D "$PGDATA" \
              --auth-local=trust \
              --auth-host=trust
          else
            echo "PostgreSQL data directory already exists"
          fi

          # Check if PostgreSQL is running, start temporarily if not
          STARTED_POSTGRES=false
          if ! ${postgresql}/bin/pg_ctl -D "$PGDATA" status > /dev/null 2>&1; then
            echo "Starting PostgreSQL temporarily for setup..."
            ${postgresql}/bin/pg_ctl \
              -D "$PGDATA" \
              -o "-k $PGHOST" \
              -l "$PGDATA/postgresql.log" \
              start
            sleep 3
            STARTED_POSTGRES=true
          fi

          # Create database if it doesn't exist
          if ! ${postgresql}/bin/psql \
               --host="$PGHOST" \
               -d postgres \
               -lqt | cut -d \| -f 1 | grep -qw example; then
            echo "Creating example database..."
            ${postgresql}/bin/createdb example --host="$PGHOST"
          else
            echo "Database 'example' already exists"
          fi

          # Load all SQL files (Pagila + custom schema)
          load-sql

          # Stop PostgreSQL if we started it
          if [ "$STARTED_POSTGRES" = true ]; then
            echo "Stopping temporary PostgreSQL instance..."
            ${postgresql}/bin/pg_ctl -D "$PGDATA" stop
          fi

          echo ""
          echo "Setup completed successfully!"
          echo "Run 'run' to start the services"
        '';

        run-postgres = pkgs.writeShellScriptBin "run-postgres" ''
          echo "Starting PostgreSQL..."

          # Set up PostgreSQL environment variables
          export PGDATA=$PWD/database/pgdata
          export PGHOST=$PWD/database/pgdata
          export PGDATABASE=example

          # Check if setup has been run
          if [ ! -f "$PGDATA/PG_VERSION" ]; then
            echo "Error: PostgreSQL not initialized. Run 'setup' first."
            exit 1
          fi

          # Start PostgreSQL if not already running
          if ${postgresql}/bin/pg_ctl -D "$PGDATA" status > /dev/null 2>&1; then
            echo "PostgreSQL is already running"
            POSTGRES_PID=$(${postgresql}/bin/pg_ctl -D "$PGDATA" status | \
              grep -o 'PID: [0-9]*' | cut -d' ' -f2)
          else
            echo "Starting PostgreSQL..."
            ${postgresql}/bin/pg_ctl \
              -D "$PGDATA" \
              -o "-k $PGHOST" \
              -l "$PGDATA/postgresql.log" \
              start
            sleep 3
            POSTGRES_PID=$(${postgresql}/bin/pg_ctl -D "$PGDATA" status | \
              grep -o 'PID: [0-9]*' | cut -d' ' -f2)

            # Start PostgreSQL log tailer
            echo "Starting PostgreSQL log monitor..."
            tail -f "$PGDATA/postgresql.log" 2>/dev/null &
            LOG_TAILER_PID=$!
          fi

          echo "PostgreSQL service started successfully"
          echo "Database: example"
          echo "Socket: $PGHOST/.s.PGSQL.5432"
          echo "Connection: postgres:///?host=$PGHOST&dbname=example"
          echo "PostgreSQL PID: $POSTGRES_PID"

          # Trap to cleanup PostgreSQL on exit
          cleanup() {
            echo "Stopping PostgreSQL..."
            if [ -n "$LOG_TAILER_PID" ] && kill -0 "$LOG_TAILER_PID" 2>/dev/null; then
              kill "$LOG_TAILER_PID" 2>/dev/null
            fi
            if [ -d "$PGDATA" ] && \
               ${postgresql}/bin/pg_ctl -D "$PGDATA" status > /dev/null 2>&1; then
              ${postgresql}/bin/pg_ctl -D "$PGDATA" stop
            fi
            exit
          }

          trap cleanup INT TERM

          # Keep the script running
          if [ -n "$LOG_TAILER_PID" ]; then
            wait $LOG_TAILER_PID
          else
            while kill -0 "$POSTGRES_PID" 2>/dev/null; do
              sleep 1
            done
          fi
        '';

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

        load-dump = pkgs.writeShellScriptBin "load-dump" ''
          export PGDATA=$PWD/database/pgdata
          export PGHOST=$PWD/database/pgdata
          export PGDATABASE=example

          # Check if database is initialized
          if [ ! -f "$PGDATA/PG_VERSION" ]; then
            echo "Error: Database not initialized. Run 'setup' first."
            exit 1
          fi

          # Check if PostgreSQL is running, start temporarily if not
          STARTED_POSTGRES=false
          if ! ${postgresql}/bin/pg_ctl -D "$PGDATA" status > /dev/null 2>&1; then
            echo "PostgreSQL is not running, starting it temporarily..."
            run-postgres &
            POSTGRES_PID=$!
            sleep 5
            STARTED_POSTGRES=true
          fi

          # Load all SQL files (Pagila + custom schema)
          load-sql

          # Stop PostgreSQL if we started it
          if [ "$STARTED_POSTGRES" = true ]; then
            echo "Stopping temporary PostgreSQL instance..."
            if [ -n "$POSTGRES_PID" ] && kill -0 "$POSTGRES_PID" 2>/dev/null; then
              kill "$POSTGRES_PID" 2>/dev/null
              wait "$POSTGRES_PID" 2>/dev/null
            fi
            ${postgresql}/bin/pg_ctl -D "$PGDATA" stop -m fast 2>/dev/null || true
          fi
        '';

        database = pkgs.writeShellScriptBin "database" ''
          export PGDATA=$PWD/database/pgdata
          export PGHOST=$PWD/database/pgdata
          export PGDATABASE=example

          echo "Connecting to database: example"
          exec ${postgresql}/bin/psql --host="$PGHOST" -d example
        '';

        run = pkgs.writeShellScriptBin "run" ''
          PORT=''${1:-9000}

          echo "Starting all services..."

          # Initialize service PIDs
          PIDS=""

          # Start PostgreSQL in background
          echo "Starting PostgreSQL service..."
          { run-postgres 2>&1 1>&3 | sed 's/.*/\x1b[31mPOSTGRES: &\x1b[0m/' >&2; } 3>&1 | sed 's/.*/\x1b[34mPOSTGRES: &\x1b[0m/' &
          POSTGRES_PID=$!
          PIDS="$PIDS $POSTGRES_PID"

          # Give PostgreSQL time to start
          sleep 5

          # Start PostgREST in background
          echo "Starting PostgREST service..."
          { run-postgrest 2>&1 1>&3 | sed 's/.*/\x1b[31mPGREST: &\x1b[0m/' >&2; } 3>&1 | sed 's/.*/\x1b[33mPGREST: &\x1b[0m/' &
          POSTGREST_PID=$!
          PIDS="$PIDS $POSTGREST_PID"
          sleep 2


          # Trap to cleanup all services on exit
          cleanup() {
            echo ""
            echo "Stopping all services..."
            for pid in $PIDS; do
              if kill -0 "$pid" 2>/dev/null; then
                echo "Stopping process $pid"
                kill "$pid" 2>/dev/null
              fi
            done

            # Wait a moment for graceful shutdown
            sleep 2

            # Force kill any remaining processes
            for pid in $PIDS; do
              if kill -0 "$pid" 2>/dev/null; then
                echo "Force stopping process $pid"
                kill -9 "$pid" 2>/dev/null
              fi
            done

            exit
          }

          trap cleanup INT TERM

          echo ""
          echo "All services started successfully!"
          echo "- PostgreSQL: Database server running on unix socket"
          echo "- PostgREST: API server on http://localhost:9080"
          echo "- Development server: http://localhost:$PORT"
          echo ""
          echo "Services running with PIDs: $PIDS"
          echo "Press Ctrl+C to stop all services."

          # Wait for all background processes
          wait
        '';

        stop = pkgs.writeShellScriptBin "stop" ''
          echo "Stopping all services..."

          export PGDATA=$PWD/database/pgdata

          # Stop PostgreSQL
          if [ -d "$PGDATA" ] && \
             ${postgresql}/bin/pg_ctl -D "$PGDATA" status > /dev/null 2>&1; then
            echo "Stopping PostgreSQL..."
            ${postgresql}/bin/pg_ctl -D "$PGDATA" stop
            echo "PostgreSQL stopped successfully"
          else
            echo "PostgreSQL is not running"
          fi

          # Stop PostgREST if running
          if pgrep -f "postgrest.*postgrest.conf" > /dev/null; then
            echo "Stopping PostgREST..."
            pkill -f "postgrest.*postgrest.conf"
            rm -f /tmp/postgrest-example.sock
            echo "PostgREST stopped successfully"
          else
            echo "PostgREST is not running"
          fi


          echo "All services stopped"
        '';

        clean = pkgs.writeShellScriptBin "clean" ''
          echo "Cleaning database..."

          export PGDATA=$PWD/database/pgdata

          # Stop services first
          stop

          # Remove PostgreSQL data directory
          if [ -d "$PGDATA" ]; then
            echo "Removing PostgreSQL data directory..."
            rm -rf "$PGDATA"
            echo "Database cleaned successfully"
          else
            echo "Database directory does not exist"
          fi

          echo ""
          echo "Run 'setup' to reinitialize the database"
        '';
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            curl
            postgresql
            postgrest
            elmPackages.elm
            nodejs
            setup
            run
            run-postgres
            run-postgrest
            load-sql
            load-dump
            database
            stop
            clean
          ];

          shellHook = ''
            export PGDATA=$PWD/database/pgdata
            export PGHOST=$PWD/database/pgdata
            export PGDATABASE=example

            # Add node_modules/.bin to PATH for npm-installed tools
            export PATH="$PWD/node_modules/.bin:$PATH"

            echo "PostgREST Admin Preview - Example Environment"
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
      });
}
