# This is your system's configuration file.
# Use this to configure your system environment (it replaces /etc/nixos/configuration.nix)
{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
  ...
}: {
  # You can import other NixOS modules here
  imports = [
    # If you want to use modules your own flake exports (from modules/nixos):
    # outputs.nixosModules.example

    # Or modules from other flakes (such as nixos-hardware):
    inputs.hardware.nixosModules.common-gpu-intel
    inputs.hardware.nixosModules.common-gpu-nvidia
    inputs.hardware.nixosModules.common-cpu-amd
    inputs.hardware.nixosModules.common-pc-ssd
    inputs.hardware.nixosModules.common-pc-laptop
    inputs.nixos-hardware.nixosModules.lenovo-thinkpad-x1-extreme-gen2

    # You can also split up your configuration and import pieces of it here:
    # ./users.nix

    # Import your generated (nixos-generate-config) hardware configuration
    ./hardware-configuration.nix

    ../../services/wiresteward
  ];

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
    registry = lib.mapAttrs (_: value: {flake = value;}) inputs;

    # This will additionally add your inputs to the system's legacy channels
    # Making legacy nix commands consistent as well, awesome!
    nixPath =
      lib.mapAttrsToList (key: value: "${key}=${value.to.path}")
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

  # some i3 needed thing
  environment.pathsToLink = ["/libexec"]; # links /libexec from derivations to /run/current-system/sw

  users.users = {
    ollie = {
      isNormalUser = true;
      initialPassword = "password";
      # passwordFile = config.sops."users.yaml/ollie/password";
      extraGroups = ["wheel" "docker" "networkmanager" "audio"];
      shell = pkgs.nushell;
    };
  };

  services.openssh = {
    enable = true;
    settings.PermitRootLogin = "no";
  };

  environment.systemPackages = with pkgs; [
    vim
    firefox
    gnupg
    pinentry-gtk2
    strongbox
    gtk3
    pavucontrol
    unzip
    feh
    arandr
  ];
  fonts.fonts = with pkgs; [(nerdfonts.override {fonts = ["Mononoki" "DroidSansMono" "Gohu"];})];

  i18n.defaultLocale = "en_GB.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    useXkbConfig = true; # use xkbOptions in tty.
  };
  time.timeZone = "Europe/London";

  # Enable sound.
  sound.enable = true;
  services.pipewire.wireplumber.enable = true;
  hardware.pulseaudio.enable = true;

  # bluetooth
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  # Enable zsa keyboards
  hardware.keyboard.zsa.enable = true;

  # tablet
  hardware.opentabletdriver = {
    enable = true;
    daemon.enable = true;
  };

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    layout = "gb";
    dpi = 180;
    upscaleDefaultCursor = true;
    videoDrivers = ["nvidia"];
    xkbOptions = "caps:ctrl_modifier";
    displayManager.lightdm = {
      enable = true;
      greeters.slick.enable = true;
    };
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
  services.dbus.enable = true;
  hardware.nvidia.modesetting.enable = true;
  hardware.nvidia.prime = {
    offload.enable = true;

    # Bus ID of the Intel GPU. You can find it using lspci, either under 3D or VGA
    intelBusId = "PCI:0:2:0";

    # Bus ID of the NVIDIA GPU. You can find it using lspci, either under 3D or VGA
    nvidiaBusId = "PCI:1:0:0";
  };

  hardware.opengl.enable = true;

  services.wiresteward.enable = true;

  security.polkit.enable = true;
  services.fprintd.enable = true;
  security.pam.services.login.fprintAuth = true;
  services.gnome.gnome-keyring.enable = true;
  security.pam.services.sddm.enableGnomeKeyring = true;

  services.pcscd.enable = true;
  programs.gnupg.agent = {
    enable = true;
    pinentryFlavor = "qt";
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

  # docker
  virtualisation.docker.enable = true;

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  system.stateVersion = "23.05";
}
