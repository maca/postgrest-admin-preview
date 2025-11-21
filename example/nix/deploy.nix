{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.pga;
  dataDir = "/var/lib/pga";
  postgrestSocket = "/var/run/pga.sock";
  serviceName = "pga";
  staticAssets = import ./static-assets.nix { inherit pkgs; };
  bluebox = import ./bluebox.nix { inherit pkgs; };

  # Fetch pre-built Elm application from GitHub releases
  # Note: This downloads without hash verification for easier CI/CD
  elmApp = pkgs.runCommand "pga-elm" {} ''
    mkdir -p $out
    ${pkgs.curl}/bin/curl -L -o $out/main.js \
      https://github.com/maca/postgrest-admin-preview/releases/latest/download/postgrest-admin.js
  '';


  # Package all static assets together
  staticBundle = pkgs.runCommand "pga-static" { } ''
    mkdir -p $out
    cp ${elmApp}/main.js $out/
    cp ${staticAssets.redoc}/redoc.standalone.js $out/
    cp ${staticAssets.icono}/icono.min.css $out/
    cp ${staticAssets.milligram}/milligram.min.css $out/
    cp ${../static/index.html} $out/
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
      isSystemUser = true;
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
        echo "Loading PGA database schema and data..."

        ${config.services.postgresql.package}/bin/psql -d ${serviceName} -f ${../database/schema.sql}
        ${config.services.postgresql.package}/bin/psql -d ${serviceName} -f ${bluebox.schema}

        TMPDIR=$(mktemp -d)
        ${pkgs.unzip}/bin/unzip -q ${bluebox.data} -d "$TMPDIR"
        ${config.services.postgresql.package}/bin/psql -d ${serviceName} -f "$TMPDIR/bluebox_dataonly_v0.4.sql"
        rm -rf "$TMPDIR"

        ${config.services.postgresql.package}/bin/psql -d ${serviceName} -f ${../database/data.sql}
        ${config.services.postgresql.package}/bin/psql -d ${serviceName} -f ${../database/permissions.sql}

        echo "Database setup completed successfully"
      '';
    };


    systemd.services.pga = {
      description = "PostgREST Admin (PGA)";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];

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
          tryFiles = "$uri $uri/ /index.html";
        };

        locations."/api/" = {
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
