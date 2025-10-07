# This is your system's configuration file.
# Use this to configure your system environment (it replaces /etc/nixos/configuration.nix)
{ inputs, pkgs, ... }:
let name = "arrakis";
in {
  # You can import other NixOS modules here
  imports = [
    # If you want to use modules your own flake exports (from modules/nixos):
    # outputs.nixosModules.example

    # Or modules from other flakes (such as nixos-hardware):
    # inputs.hardware.nixosModules.common-cpu-intel
    inputs.nixos-hardware.nixosModules.lenovo-thinkpad-x1-nano-gen1

    # You can also split up your configuration and import pieces of it here:
    # ./users.nix

    # Import your generated (nixos-generate-config) hardware configuration
    ./hardware-configuration.nix

    ../../modules/nixos
  ];

  config = {
    # ensure this is set to the same value as the one in the "hosts" module
    networking.hostName = name;

    modules.laptop = {
      enable = true;
      networkInterface = "wlp0s20f3";
    };
    modules.x.enable = true;
    services.xserver = { videoDrivers = [ "displaylink" "modesetting" ]; };
    modules.work.enable = true;

    # Bootloader
    boot.loader.systemd-boot.enable = true;
    boot.loader.efi.canTouchEfiVariables = true;

    # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
    system = {
      stateVersion = "25.05";
      activationScripts.diff = ''
            if [[ -e //run/current/system ]]; then
              ${pkgs.nix}/bin/nix store diff-closures /run/current-system "$systemConfig"
        fi
      '';
    };
  };
}
