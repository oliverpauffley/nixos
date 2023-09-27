{ config, lib, pkgs, ... }:

{

  users.groups.multimedia = { };
  users.users.ollie.extraGroups = [ "multimedia" ];

  systemd.tmpfiles.rules = [ "d /mnt/media 0770 - multimedia - -" ];

  # jellyfin config
  nixpkgs.config.packageOverrides = pkgs: {
    vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };
  };
  hardware.opengl = {
    enable = true;
    extraPackages = with pkgs; [
      intel-media-driver
      vaapiIntel
      vaapiVdpau
      libvdpau-va-gl
      intel-compute-runtime # OpenCL filter support (hardware tonemapping and subtitle burn-in)
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
  };
  networking.firewall.allowedTCPPorts = [ 8080 ];
}
