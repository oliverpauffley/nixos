{
  flake.modules.nixos.media = { pkgs, inputs, ... }: {
    nixpkgs.overlays = [ inputs.self.overlays.default ];
    environment.systemPackages = with pkgs; [ ffmpeg local.dovi-convert ];

    users.groups.multimedia = { };
    users.users.ollie.extraGroups = [ "multimedia" ];

    systemd.tmpfiles.rules = [ "d /mnt/media 0770 - multimedia - -" ];

    # jellyfin graphics config
    nixpkgs.config.packageOverrides = pkgs: {
      vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };
    };
    hardware.graphics.enable = true;
    hardware.opengl = {
      extraPackages = with pkgs; [
        intel-media-driver
        vaapiIntel
        vaapiVdpau
        libvdpau-va-gl
        intel-compute-runtime # OpenCL filter support (hardware tonemapping and subtitle burn-in)
        intel-media-sdk # QSV up to 11th gen
      ];
    };

    services = {
      jellyfin = {
        enable = true;
        openFirewall = true;
        group = "multimedia";
      };
      # movies and tv
      sonarr = {
        enable = true;
        group = "multimedia";
        openFirewall = true;
      };
      radarr = {
        enable = true;
        group = "multimedia";
        openFirewall = true;
      };
      prowlarr = {
        package = pkgs.prowlarr;
        enable = true;
        openFirewall = true;
      };
      deluge = {
        enable = true;
        group = "multimedia";
        web = {
          enable = true;
          openFirewall = true;
        };
        dataDir = "/mnt/media/torrent";
      };
      sabnzbd = {
        enable = true;
        group = "multimedia";
      };
      plex = {
        enable = true;
        openFirewall = true;
        group = "multimedia";
      };
      suwayomi-server = {
        enable = true;
        openFirewall = true;
        settings = { port = "1111"; };
        group = "multimedia";
      };
      homepage-dashboard = {
        enable = true;
        listenPort = 8082;
        allowedHosts =
          "localhost:8082,127.0.0.1:8082,192.168.1.100:8082,dashboard.home.lab";
        openFirewall = true;
        package = pkgs.homepage-dashboard;
        services = [{
          "Media" = [
            {
              "Jellyfin" = {
                description = "media player";
                href = "http://192.168.1.100:8096/";
              };
            }
            {
              "sabnzbd" = {
                description = "nzb downloader";
                href = "http://192.168.1.100:8080/";
              };
            }
            {
              "radarr" = {
                description = "film downloader";
                href = "http://192.168.1.100:7878/";
              };
            }
            {
              "sonarr" = {
                description = "tv downloader";
                href = "http://192.168.1.100:8989/";
              };
            }
            {
              "prowlarr" = {
                description = "index manager";
                href = "http://192.168.1.100:9696/";
              };
            }
            {
              "deluge" = {
                description = "torrent downloader";
                href = "http://192.168.1.100:58846/";
              };
            }
            {
              "plex" = {
                description = "media player";
                href = "http://192.168.1.100:32400/web";
              };
            }
          ];
        }];
      };
    };

    networking.firewall.allowedTCPPorts = [ 8080 ];
    services.logrotate.enable = true;
  };
}
