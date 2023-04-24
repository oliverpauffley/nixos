# This is your system's configuration file.
# Use this to configure your system environment (it replaces /etc/nixos/configuration.nix)

{ inputs, outputs, lib, config, pkgs, ... }:
let
  nvidia-offload = pkgs.writeShellScriptBin "nvidia-offload"
    "  export __NV_PRIME_RENDER_OFFLOAD=1\n  export __NV_PRIME_RENDER_OFFLOAD_PROVIDER=NVIDIA-G0\n  export __GLX_VENDOR_LIBRARY_NAME=nvidia\n  export __VK_LAYER_NV_optimus=NVIDIA_only\n  exec \"$@\"\n";
in
{
  # You can import other NixOS modules here
  imports = [
    # If you want to use modules your own flake exports (from modules/nixos):
    # outputs.nixosModules.example

    # Or modules from other flakes (such as nixos-hardware):
    inputs.hardware.nixosModules.common-cpu-amd
    inputs.hardware.nixosModules.common-gpu-intel
    inputs.hardware.nixosModules.common-gpu-nvidia
    inputs.hardware.nixosModules.common-pc-ssd
    inputs.hardware.nixosModules.common-hidpi
    inputs.hardware.nixosModules.common-pc-laptop

    # You can also split up your configuration and import pieces of it here:
    # ./users.nix

    # Import your generated (nixos-generate-config) hardware configuration
    ./hardware-configuration.nix

    ./services/wiresteward

  ];

  # sops secret setups
  #sops.defaultSopsFile = ../secrets/users.yaml;
  #sops.secrets."users.yaml/ollie/password".neededForUsers = true;

  nixpkgs = {
    # You can add overlays here
    overlays = [
      # Add overlays your own flake exports (from overlays and pkgs dir):
      outputs.overlays.additions
      outputs.overlays.modifications
      outputs.overlays.unstable-packages

      # You can also add overlays exported from other flakes:
      # neovim-nightly-overlay.overlays.default

      # Or define it inline, for example:
      # (final: prev: {
      #   hi = final.hello.overrideAttrs (oldAttrs: {
      #     patches = [ ./change-hello-to-hi.patch ];
      #   });
      # })
    ];
    # Configure your nixpkgs instance
    config = {
      # Disable if you don't want unfree packages
      allowUnfree = true;
    };
  };

  nix = {
    # This will add each flake input as a registry
    # To make nix3 commands consistent with your flake
    registry = lib.mapAttrs (_: value: { flake = value; }) inputs;

    # This will additionally add your inputs to the system's legacy channels
    # Making legacy nix commands consistent as well, awesome!
    nixPath = lib.mapAttrsToList (key: value: "${key}=${value.to.path}")
      config.nix.registry;

    settings = {
      # Enable flakes and new 'nix' command
      experimental-features = "nix-command flakes";
      # Deduplicate and optimize nix store
      auto-optimise-store = true;
    };
  };

  networking.hostName = "arrakis";
  networking.networkmanager.enable = true;
  programs.nm-applet.enable = true;

  # Bootloader
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";

  users.users = {
    ollie = {
      isNormalUser = true;
      initialPassword = "password";
      # passwordFile = config.sops."users.yaml/ollie/password";
      extraGroups = [ "wheel" "docker" "networkmanager" "audio" ];
    };
  };

  services.openssh = {
    enable = true;
    permitRootLogin = "no";
  };

  environment.systemPackages = with pkgs; [
    nvidia-offload
    vim
    firefox
    gnupg
    pinentry-curses
    gtk3
  ];

  i18n.defaultLocale = "en_GB.UTF-8";

  # Enable sound.
  sound.enable = true;

  services.pipewire = {
    enable = true;
    alsa.enable = true;
    pulse.enable = true;
  };

  # Enable zsa keyboards
  hardware.keyboard.zsa.enable = true;
  hardware = { opengl = { enable = true; }; };

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    layout = "gb";
    dpi = 180;
    videoDrivers = [ "nvidia" ];
    displayManager.lightdm.enable = true;
    windowManager.i3.enable = true;

    libinput = {
      enable = true;

      touchpad = {
        clickMethod = "buttonareas";
        disableWhileTyping = true;
        middleEmulation = true;
        tapping = true;
        additionalOptions = ''
          Option "PalmDetection" "on"
          Option "TappingButtonMap" "lmr"
        '';
      };
    };
  };

  # nvidia prime settings
  hardware.nvidia.modesetting.enable = true;

  hardware.nvidia.prime = {
    offload.enable = true;

    # Bus ID of the Intel GPU. You can find it using lspci, either under 3D or VGA
    intelBusId = "PCI:0:2:0";

    # Bus ID of the NVIDIA GPU. You can find it using lspci, either under 3D or VGA
    nvidiaBusId = "PCI:1:0:0";
  };

  services.wiresteward.enable = true;

  security.polkit.enable = true;
  services.fprintd.enable = true;
  security.pam.services.login.fprintAuth = true;
  services.gnome.gnome-keyring.enable = true;
  security.pam.services.sddm.enableGnomeKeyring = true;

  services.pcscd.enable = true;
  programs.gnupg.agent = {
    enable = true;
    pinentryFlavor = "curses";
    enableSSHSupport = true;
  };

  # printing
  services.printing.enable = true;

  # power managment
  services.upower.enable = true;
  services.tlp = {
    enable = true;
    settings = {
      CPU_SCALING_GOVERNOR_ON_AC = "performance";
      CPU_SCALING_GOVERNOR_ON_BAT = "powersave";
    };
  };


  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  system.stateVersion = "22.11";
}