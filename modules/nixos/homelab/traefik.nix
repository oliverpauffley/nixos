{ lib, config, ... }:
let
  domain = "home.lab";
  localIp = config.modules.homelab.currentHost.ipv4;
  cfg = config.modules.homelab.traefik;
in {

  options.modules.homelab.traefik = {
    enable = lib.mkEnableOption "Enable traefik load balancer";
  };

  config = lib.mkIf cfg.enable {

    networking.firewall.allowedTCPPorts = [ 80 ];

    services.traefik = {
      enable = true;

      staticConfigOptions = {
        api.dashboard = true;
        api.insecure = false;

        log = {
          level = "DEBUG";
          filePath = "/var/lib/traefik/traefik.log";
        };

        accessLog = { filePath = "/var/lib/traefik/access.log"; };

        entryPoints = { local = { address = "${localIp}:80"; }; };
      };

      # Dashboard
      dynamicConfigOptions.http = {
        routers = {
          dashboard = {
            rule = lib.mkDefault "Host(`dashboard.${domain}`)";
            service = "homepage-dashboard";
            entryPoints = [ "local" ];

          };
          plex = {
            rule = lib.mkDefault "Host(`plex.${domain}`)";
            service = "plex";
            entryPoints = [ "local" ];
          };
          prowlarr = {
            rule = lib.mkDefault "Host(`prowlarr.${domain}`)";
            service = "prowlarr";
            entryPoints = [ "local" ];
          };
          radarr = {
            rule = lib.mkDefault "Host(`radarr.${domain}`)";
            service = "radarr";
            entryPoints = [ "local" ];
          };
          sonarr = {
            rule = lib.mkDefault "Host(`sonarr.${domain}`)";
            service = "sonarr";
            entryPoints = [ "local" ];
          };
        };

        services = {
          plex = {
            loadBalancer = { servers = [{ url = "http://localhost:32400"; }]; };
          };
          prowlarr = {
            loadBalancer = { servers = [{ url = "http://localhost:9696"; }]; };
          };
          radarr = {
            loadBalancer = { servers = [{ url = "http://localhost:7878"; }]; };
          };
          sonarr = {
            loadBalancer = { servers = [{ url = "http://localhost:8989"; }]; };
          };
          homepage-dashboard = {
            loadBalancer = { servers = [{ url = "http://localhost:8082"; }]; };
          };
        };
      };
    };
  };
}
