{ lib, ... }: {
  flake.modules.nixos."hosts/giedi-prime" = { pkgs, config, ... }: {
    nixpkgs.hostPlatform = "x86_64-linux";

    # TODO move these?
    services.xserver = { videoDrivers = [ "nvidia" ]; };
    powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
    networking.hostName = "giedi-prime";

    hardware.opengl = {
      extraPackages = with pkgs; [
        nvidia-vaapi-driver
        vaapiVdpau
        libvdpau-va-gl
      ];
    };
    hardware.nvidia = {
      open = true;
      modesetting.enable = true;
      package = config.boot.kernelPackages.nvidiaPackages.mkDriver {
        version = "580.65.06";
        settingsSha256 = "sha256-9PWmj9qG/Ms8Ol5vLQD3Dlhuw4iaFtVHNC0hSyMCU24=";
        usePersistenced = false;
      };
      nvidiaSettings = false;
    };
    boot.kernelPackages = pkgs.linuxPackages_6_16;

    boot = {
      loader.systemd-boot.enable = true;
      loader.efi.canTouchEfiVariables = true;

      initrd.availableKernelModules =
        [ "xhci_pci" "ahci" "nvme" "usb_storage" "usbhid" "sd_mod" ];
      initrd.kernelModules = [ ];
      kernelModules = [ "kvm-intel" ];
      extraModulePackages = [ ];
    };
  };
}
