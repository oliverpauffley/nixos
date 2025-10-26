{ lib, ... }: {
  flake.hosts."caladan" = {
    description = "homelab";
    ipv4 = "192.168.1.100";
    dnsalias = [ "caladan" "sonarr" "radarr" "dashboard" "plex" "film" "tv" ];
    isDNS = true;
  };
  flake.modules.nixos."hosts/caladan" = { pkgs, config, ... }: {
    nixpkgs.hostPlatform = "x86_64-linux";

    # TODO move these?
    powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
    networking.hostName = "caladan";
    networking.hostId = "347b26dc"; # required for zfs
    hardware.cpu.intel.updateMicrocode = lib.mkDefault true;
    hardware.enableRedistributableFirmware = true;

    hardware.opengl = {
      extraPackages = with pkgs; [
        nvidia-vaapi-driver
        vaapiVdpau
        libvdpau-va-gl
      ];
    };
    hardware.opengl = { driSupport32Bit = true; };

    nixpkgs.config.nvidia.acceptLicense = true;

    services.xserver.videoDrivers = [ "nvidia" ];
    hardware.nvidia = {
      modesetting.enable = true;
      powerManagement.enable = false;
      powerManagement.finegrained = false;
      open = false;
      nvidiaSettings = true;
      package = config.boot.kernelPackages.nvidiaPackages.legacy_470;
    };
    services.openssh.settings.PermitRootLogin = lib.mkForce "yes";
    security.pam.enableSSHAgentAuth = true;
    services.openssh.enable = true;

    boot = {
      loader.systemd-boot.enable = true;
      loader.efi.canTouchEfiVariables = true;

      initrd.availableKernelModules =
        [ "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod" ];
      zfs.forceImportRoot = false;
      initrd.kernelModules = [ "dm-snapshot" ];
      extraModulePackages = [ ];
    };
  };
}
