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
    #   inputs.hardware.nixosModules.common-gpu-intel
    inputs.hardware.nixosModules.common-gpu-nvidia
    inputs.hardware.nixosModules.common-pc-ssd
    inputs.hardware.nixosModules.common-hidpi
    inputs.hardware.nixosModules.common-pc-laptop
    inputs.nixos-hardware.nixosModules.lenovo-thinkpad-x1-extreme-gen2

    #inputs.stylix.nixosModules.stylix
    # You can also split up your configuration and import pieces of it here:
    # ./users.nix

    inputs.hyprland.nixosModules.default
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
      shell = pkgs.fish;
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
    pinentry-qt
    strongbox
    gtk3
    pavucontrol
    unzip
    feh
  ];

  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true;
    dedicatedServer.openFirewall = true;
  };
  i18n.defaultLocale = "en_GB.UTF-8";
  time.timeZone = "Europe/London";

  # Enable sound.
  sound.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    pulse.enable = true;
    media-session.config.bluez-monitor.rules = [
      {
        # Matches all cards
        matches = [{ "device.name" = "~bluez_card.*"; }];
        actions = {
          "update-props" = {
            "bluez5.reconnect-profiles" = [ "hfp_hf" "hsp_hs" "a2dp_sink" ];
            # mSBC is not expected to work on all headset + adapter combinations.
            "bluez5.msbc-support" = true;
            # SBC-XQ is not expected to work on all headset + adapter combinations.
            "bluez5.sbc-xq-support" = true;
          };
        };
      }
      {
        matches = [
          # Matches all sources
          {
            "node.name" = "~bluez_input.*";
          }
          # Matches all outputs
          { "node.name" = "~bluez_output.*"; }
        ];
      }
    ];
  };

  # bluetooth
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  # Enable zsa keyboards
  hardware.keyboard.zsa.enable = true;

  programs.hyprland.enable = true;

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    layout = "gb";
    dpi = 180;
    displayManager.gdm.enable = true;
    videoDrivers = [ "nvidia" ];
    xkbOptions = "caps:ctrl_modifier";
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

  # TODO dual monitors?
  # nvidia prime settings
  hardware.nvidia.modesetting.enable = true;
  hardware.nvidia.prime = {
    offload.enable = true;

    # Bus ID of the Intel GPU. You can find it using lspci, either under 3D or VGA
    intelBusId = "PCI:0:2:0";

    # Bus ID of the NVIDIA GPU. You can find it using lspci, either under 3D or VGA
    nvidiaBusId = "PCI:1:0:0";
  };

  # boot with graphics card for external display
  specialisation = {
    external-display.configuration = {
      system.nixos.tags = [ "external-display" ];
      hardware.nvidia.prime.offload.enable = lib.mkForce false;
      hardware.nvidia.powerManagement.enable = lib.mkForce false;
    };
  };
  hardware = { opengl = { enable = true; }; };

  services.wiresteward.enable = true;

  # TODO figure out saving the password for emacs logins
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

  # stylix themeing
  # stylix = {
  #   autoEnable = true;
  #   polarity = "dark";
  #   image = ./serra.jpg;
  #   # image = pkgs.fetchurl {
  #   #   url =
  #   #     "https://c4.wallpaperflare.com/wallpaper/212/579/800/graphic-design-fantasy-landscape-retro-wave-pixeles-wallpaper-preview.jpg";
  #   #   sha256 = "JJcYfrB+lfqQPiQHoBDE3LVps86MjBj6XZyv06Rv6PI=";
  #   # };
  #   fonts = {
  #     monospace = {
  #       package = pkgs.nerdfonts.override { fonts = [ "Mononoki" ]; };
  #       name = "mononoki Nerd Font";
  #     };
  #     serif = config.stylix.fonts.monospace;

  #     sansSerif = config.stylix.fonts.monospace;

  #     emoji = config.stylix.fonts.monospace;
  #   };
  # };

  # docker
  virtualisation.docker.enable = true;

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  system.stateVersion = "22.11";
}
