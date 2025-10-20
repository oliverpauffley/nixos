{ lib, config, ... }:
let name = "arrakis";
in {
  flake.hosts."${name}" = {
    description = "work laptop";
    ipv4 = "192.168.1.159";
    dnsalias = [ name ];
  };
  flake.modules.nixos."hosts/${name}" = {
    nixpkgs.hostPlatform = "x86_64-linux";

    services.xserver = { videoDrivers = [ "displaylink" "modesetting" ]; };
    powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
    networking.hostName = name;
    hardware.cpu.intel.updateMicrocode = lib.mkDefault true;
    hardware.enableRedistributableFirmware = true;

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
