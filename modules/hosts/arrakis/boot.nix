{ lib, config, ... }: {
  flake.hosts."arrakis" = {
    description = "work laptop";
    ipv4 = "192.168.1.159";
    dnsalias = [ "arrakis" ];
  };
  flake.modules.nixos."hosts/arrakis" = {
    nixpkgs.hostPlatform = "x86_64-linux";

    services.xserver = { videoDrivers = [ "displaylink" "modesetting" ]; };
    powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
    networking.hostName = "arrakis";

    boot = {
      loader.systemd-boot.enable = true;
      loader.efi.canTouchEfiVariables = true;

      initrd.availableKernelModules =
        [ "xhci_pci" "thunderbolt" "nvme" "usb_storage" "sd_mod" ];
      initrd.kernelModules = [ ];
      kernelModules = [ "kvm-intel" ];
      extraModulePackages = [ ];
    };
  };
}
