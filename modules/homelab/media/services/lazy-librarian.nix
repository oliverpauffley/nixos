{ lib, ... }:
{
  flake.modules.nixos.lazy-librarian =
    {
      config,
      lib,
      pkgs,
      ...
    }:

    let
      cfg = config.services.lazylibrarian;
    in
    {
      options.services.lazylibrarian = {
        enable = lib.mkEnableOption "LazyLibrarian book, magazine and audiobook manager";

        package = lib.mkOption {
          type = lib.types.package;
          default = pkgs.local.lazy-librarian;
          description = "The LazyLibrarian package to use.";
        };

        port = lib.mkOption {
          type = lib.types.port;
          default = 5299;
          description = "Port the LazyLibrarian web interface listens on.";
        };

        host = lib.mkOption {
          type = lib.types.str;
          default = "0.0.0.0";
          description = ''
            Address the web interface binds to
            (config.ini http_host).
          '';
        };

        dataDir = lib.mkOption {
          type = lib.types.str;
          default = "/var/lib/lazylibrarian";
          description = ''
            Directory holding config.ini, the database and the cache.
          '';
        };

        user = lib.mkOption {
          type = lib.types.str;
          default = "lazylibrarian";
          description = ''
            User the service runs as.
            A system user is created when left at the default.
          '';
        };

        group = lib.mkOption {
          type = lib.types.str;
          default = "lazylibrarian";
          description = ''
            Group the service runs as.
            A group is created when left at the default.
          '';
        };

        openFirewall = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = "Whether to open port in the firewall.";
        };

        environmentFile = lib.mkOption {
          type = lib.types.nullOr lib.types.path;
          default = null;
          description = ''
            Path to an environment file loaded into the service.
            Use it to supply variables referenced by secretSettings.
          '';
        };

        secretSettings = lib.mkOption {
          type = lib.types.attrsOf lib.types.str;
          default = { };

          example = lib.literalExpression ''
            {
              "API.api_key" = "LAZYLIBRARIAN_API_KEY";
            }
          '';

          description = ''
            Map of Section.key entries in config.ini to the name of
            the environment variable containing their value.
          '';
        };

        settings = lib.mkOption {
          type = (pkgs.formats.ini { }).type;
          default = { };

          example = lib.literalExpression ''
            {
              API.book_api = "OpenLibrary";
              Git.auto_update = false;
              General.loglevel = 1;
              WebServer.http_root = "/lazylibrarian";
            }
          '';

          description = ''
            Declarative settings merged into LazyLibrarian's config.ini
            on every start.
          '';
        };
      };

      config = lib.mkIf cfg.enable {
        services.lazylibrarian.settings.WebServer = {
          http_port = lib.mkDefault cfg.port;
          http_host = lib.mkDefault cfg.host;
        };

        systemd.services.lazylibrarian = {
          description = "LazyLibrarian book manager";

          wantedBy = [ "multi-user.target" ];
          after = [ "network-online.target" ];
          wants = [ "network-online.target" ];

          startLimitIntervalSec = 0;

          serviceConfig = {
            Type = "simple";

            User = cfg.user;
            Group = cfg.group;

            StateDirectory = lib.mkIf (cfg.dataDir == "/var/lib/lazylibrarian") "lazylibrarian";

            WorkingDirectory = cfg.dataDir;

            ExecStart = lib.concatStringsSep " " [
              (lib.getExe cfg.package)
              "--datadir ${cfg.dataDir}"
              "--port ${toString cfg.port}"
              "--nolaunch"
            ];

            EnvironmentFile = lib.optional (cfg.environmentFile != null) cfg.environmentFile;

            Restart = "always";
            RestartSec = "5s";
          };
        };

        networking.firewall.allowedTCPPorts = lib.mkIf cfg.openFirewall [ cfg.port ];

        users.users = lib.mkIf (cfg.user == "lazylibrarian") {
          lazylibrarian = {
            isSystemUser = true;
            group = cfg.group;
            description = "LazyLibrarian daemon user";
            home = cfg.dataDir;
          };
        };

        users.groups = lib.mkIf (cfg.group == "lazylibrarian") {
          lazylibrarian = { };
        };
      };
    };
}
