{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.pga;
  dataDir = "/var/lib/pga";
  postgrestSocket = "/run/pga/pga.sock";
  serviceName = "pga";
  staticAssets = import ./static-assets.nix { inherit pkgs; };
  bluebox = import ./bluebox.nix { inherit pkgs; };

  # Fetch pre-built Elm application from GitHub releases
  elmApp = pkgs.stdenv.mkDerivation {
    pname = "pga-elm";
    version = "latest";

    src = pkgs.fetchurl {
      url = "https://github.com/maca/postgrest-admin-preview/releases/latest/download/postgrest-admin.js";
      sha256 = "sha256-NSG4in1Ijwsq1iKDuCzcDZe7HETY9f6rhTPsiajHg3o=";
    };

    dontUnpack = true;

    installPhase = ''
      mkdir -p $out
      cp $src $out/main.js
    '';
  };


  # Package all static assets together
  staticBundle = pkgs.runCommand "pga-static" { } ''
        mkdir -p $out
        cp ${elmApp}/main.js $out/main.js
        cp ${staticAssets.redoc}/redoc.standalone.js $out/redoc.standalone.js
        cp ${staticAssets.icono}/icono.min.css $out/icono.min.css
        cp ${staticAssets.milligram}/milligram.min.css $out/milligram.min.css
        cp ${../static/app.css} $out/app.css

        cat > $out/index.html <<'EOF'
    <!DOCTYPE HTML>
    <html>
    <head>
      <meta charset="UTF-8">
      <title>Main</title>
      <style>body { padding: 0; margin: 0; }</style>
      <link rel="stylesheet" href="/icono.min.css" />
      <link rel="stylesheet" href="/milligram.min.css" />
      <link rel="stylesheet" href="/app.css" type="text/css" media="screen" />
      <script src="/main.js"></script>
    </head>

    <body>
        <script type="text/javascript">
            console.log("token: ", sessionStorage.getItem("jwt"))
            const app = Elm.Main.init({
                flags: {
                    jwt: sessionStorage.getItem("jwt"),
                    host: "http://pga-api.bitmunge.com",
                    clientHeaders: {
                      "Accept-Profile": "bluebox",
                      "Content-Profile": "bluebox"
                    }
                }
            })

            app.ports.loggedIn.subscribe(jwt => {
                console.log("got token: ", jwt)
                sessionStorage.setItem("jwt", jwt)
            });

            app.ports.loggedOut.subscribe(_ => {
                sessionStorage.removeItem("jwt")
            });
        </script>
    <body>
    EOF
  '';


  # PostgREST config file
  postgrestConf = pkgs.writeText "postgrest.conf" ''
    db-uri = "postgres:///?host=${cfg.databaseSocket}&dbname=${serviceName}"
    db-schemas = "public, bluebox"
    db-anon-role = "web_anon"
    jwt-secret = "DL+P8+muauKgOSqRKqIKMkjcUpLZ5ajXScgA965i/Bg="
    server-unix-socket = "${postgrestSocket}"
    log-level = "info"
  '';
in
{
  options.services.pga = {
    enable = mkEnableOption "PostgREST Admin (PGA) service";

    databaseSocket = mkOption {
      type = types.str;
      default = "/run/postgresql";
      description = "PostgreSQL unix socket directory";
    };
  };

  config = mkIf cfg.enable {
    users.users.pga = {
      isNormalUser = true;
      # isSystemUser = true;
      group = "web";
      home = dataDir;
      createHome = true;
      description = "PostgREST Admin service user";
    };
    users.groups.web = { };


    systemd.services.pga-setup = {
      description = "Setup PGA database schema and load data";
      after = [ "postgresql.service" ];
      requires = [ "postgresql.service" ];

      serviceConfig = {
        Type = "oneshot";
        User = serviceName;
        Group = "web";
      };

      script = ''
        set -xeuo pipefail

        echo "Loading PGA database schema and data..."

        ${config.services.postgresql.package}/bin/psql -v ON_ERROR_STOP=1 -d ${serviceName} -f ${../database/schema.sql}
        ${config.services.postgresql.package}/bin/psql -v ON_ERROR_STOP=1 -d ${serviceName} -f ${bluebox.schema}

        TMPDIR=$(mktemp -d)
        ${pkgs.unzip}/bin/unzip -q ${bluebox.data} -d "$TMPDIR"
        ${config.services.postgresql.package}/bin/psql -v ON_ERROR_STOP=1 -d ${serviceName} -f "$TMPDIR/bluebox_dataonly_v0.4.sql"
        rm -rf "$TMPDIR"

        ${config.services.postgresql.package}/bin/psql -v ON_ERROR_STOP=1 -d ${serviceName} -f ${../database/data.sql}
        ${config.services.postgresql.package}/bin/psql -v ON_ERROR_STOP=1 -d ${serviceName} -f ${../database/permissions.sql}

        echo "Database setup completed successfully"
      '';
    };


    systemd.services.pga = {
      description = "PostgREST Admin (PGA)";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" "postgresql.service" "pga-setup.service" ];
      requires = [ "pga-setup.service" ];

      serviceConfig = {
        Type = "simple";
        User = serviceName;
        Group = "web";

        ExecStart = "${pkgs.postgrest}/bin/postgrest ${postgrestConf}";

        Restart = "on-failure";
        RestartSec = "5s";

        # Security
        PrivateTmp = true;
        NoNewPrivileges = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        ReadOnlyPaths = [ dataDir ];
        RuntimeDirectory = serviceName;
        RuntimeDirectoryMode = "0755";
      };
    };


    services.postgresql = {
      enable = true;
      ensureDatabases = [ serviceName ];
      ensureUsers = [{
        name = serviceName;
        ensureDBOwnership = true;
      }];
    };


    services.nginx = {
      enable = true;

      virtualHosts."pga.bitmunge.com" = {
        root = staticBundle;

        locations."/" = {
          tryFiles = "$uri @fallback";
        };

        locations."@fallback" = {
          extraConfig = ''
            # Only serve index.html if the request has no file extension
            if ($uri !~ \.[a-zA-Z0-9]+$) {
              rewrite ^ /index.html break;
            }
            return 404;
          '';
        };
      };

      virtualHosts."pga-api.bitmunge.com" = {
        locations."/" = {
          proxyPass = "http://unix:${postgrestSocket}";
          extraConfig = ''
            proxy_set_header Host $host;
            proxy_set_header X-Real-IP $remote_addr;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_set_header X-Forwarded-Proto $scheme;
          '';
        };
      };
    };

    users.users.nginx.extraGroups = [ "web" ];
  };
}
