{ config, ... }: {
  flake.modules.nixos.media =
    {
      pkgs,
      inputs,
      ...
    }:
    let
      grp = "multimedia";
    in
    {
      imports = [
        config.flake.modules.nixos.lazy-librarian
      ];
      nixpkgs.overlays = [ inputs.self.overlays.default ];
      environment.systemPackages = with pkgs; [
        ffmpeg
        local.dovi-convert
        borgbackup
      ];

      users.groups.${grp} = { };
      users.users.ollie.extraGroups = [ grp ];

      systemd.tmpfiles.rules = [ "d /mnt/media 0770 - ${grp} - -" ];

      # jellyfin graphics config
      hardware.graphics = {
        enable = true;
        extraPackages = with pkgs; [
          intel-media-driver
          libvdpau-va-gl
          intel-compute-runtime # OpenCL filter support (hardware tonemapping and subtitle burn-in)
        ];
      };

      services = {
        # movies and tv
        sonarr = {
          enable = true;
          group = grp;
          openFirewall = true;
        };
        radarr = {
          enable = true;
          group = grp;
          openFirewall = true;
        };
        prowlarr = {
          package = pkgs.prowlarr;
          enable = true;
          openFirewall = true;
        };
        deluge = {
          enable = true;
          group = grp;
          web = {
            enable = true;
            openFirewall = true;
          };
          dataDir = "/mnt/media/torrent";
        };

        calibre-server = {
          enable = true;
          openFirewall = true;
          port = 8009;
          libraries = [
            "/mnt/media/books"
          ];
        };

        sabnzbd = {
          enable = true;
          group = grp;
        };
        plex = {
          enable = true;
          openFirewall = true;
          group = grp;
        };

        lazylibrarian = {
          enable = true;
          port = 5299;
          group = grp;
          openFirewall = true;
          settings = {
            Git.auto_update = false;
          };
        };

        homepage-dashboard = {
          enable = true;
          listenPort = 8082;
          allowedHosts = "localhost:8082,127.0.0.1:8082,192.168.0.100:8082,dashboard.home.lab";
          openFirewall = true;
          package = pkgs.homepage-dashboard;
          services = [
            {
              "Media" = [
                {
                  "sabnzbd" = {
                    description = "nzb downloader";
                    href = "http://192.168.0.100:8080/";
                  };
                }
                {
                  "radarr" = {
                    description = "film downloader";
                    href = "http://192.168.0.100:7878/";
                  };
                }
                {
                  "sonarr" = {
                    description = "tv downloader";
                    href = "http://192.168.0.100:8989/";
                  };
                }
                {
                  "prowlarr" = {
                    description = "index manager";
                    href = "http://192.168.0.100:9696/";
                  };
                }
                {
                  "deluge" = {
                    description = "torrent downloader";
                    href = "http://192.168.0.100:8112/";
                  };
                }
                {
                  "plex" = {
                    description = "media player";
                    href = "http://192.168.0.100:32400/web";
                  };
                }
                {
                  "lazylibrarian" = {
                    description = "books downloading";
                    href = "http://192.168.0.100:5299";
                  };
                }
              ];
            }
          ];
        };
      };

      networking.firewall.allowedTCPPorts = [ 8080 ];
      services.logrotate.enable = true;
    };
}
