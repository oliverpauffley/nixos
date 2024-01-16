# This is your system's configuration file.
# Use this to configure your system environment (it replaces /etc/nixos/configuration.nix)
{ inputs, outputs, lib, config, pkgs, ... }: {
  # You can import other NixOS modules here
  imports = [
    # If you want to use modules your own flake exports (from modules/nixos):
    # outputs.nixosModules.example

    # Or modules from other flakes (such as nixos-hardware):
    inputs.hardware.nixosModules.common-gpu-intel
    inputs.hardware.nixosModules.common-cpu-intel
    inputs.nixos-hardware.nixosModules.lenovo-thinkpad-x1-nano-gen1

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
      # nix user repo
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
  networking.nameservers = [ "9.9.9.9" "1.1.1.1" "8.8.8.8" ];
  networking.networkmanager.dns = "none";
  networking.dhcpcd.extraConfig = "nohook resolv.conf";
  services.resolved.enable = false;

  programs.nm-applet.enable = true;

  # Bootloader
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # some i3 needed thing
  environment.pathsToLink =
    [ "/libexec" ]; # links /libexec from derivations to /run/current-system/sw

  users.users = {
    ollie = {
      isNormalUser = true;
      initialPassword = "password";
      # passwordFile = config.sops."users.yaml/ollie/password";
      extraGroups = [ "wheel" "docker" "networkmanager" "audio" ];
      shell = pkgs.fish;
    };
  };
  # really really set the shell
  environment.shells = [ pkgs.fish ];

  # 1password setup
  programs._1password.enable = true;
  programs._1password-gui.enable = true;
  programs._1password-gui.polkitPolicyOwners = [ "ollie" ];

  services.openssh = {
    enable = true;
    settings.PermitRootLogin = "no";
  };

  programs.fish.enable = true;
  environment.systemPackages = with pkgs; [
    dict
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
    syncthing
    polkit_gnome
    mu
  ];
  fonts.packages = with pkgs;
    [ (nerdfonts.override { fonts = [ "Mononoki" "DroidSansMono" "Gohu" ]; }) ];
  fonts.fontDir.enable = true;
  fonts.fontDir.decompressFonts = true;

  programs.steam = {
    enable = true;
    remotePlay.openFirewall =
      true; # Open ports in the firewall for Steam Remote Play
    dedicatedServer.openFirewall =
      true; # Open ports in the firewall for Source Dedicated Server
  };

  i18n.defaultLocale = "en_GB.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    useXkbConfig = true; # use xkbOptions in tty.
  };
  time.timeZone = "Europe/London";

  # enable dictionary{
  environment.etc."dict.conf".text = "server dict.org";

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
    videoDrivers = [ "modesetting" ];
    xkbOptions = "caps:ctrl_modifier";
    displayManager.gdm.enable = true;
    windowManager.i3.enable = true;

    desktopManager = { xterm.enable = false; };

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

  hardware.opengl.enable = true;

  services.wiresteward.enable = true;

  # start gnome polkit in i3
  systemd = {
    user.services.polkit-gnome-authentication-agent-1 = {
      description = "polkit-gnome-authentication-agent-1";
      wantedBy = [ "graphical-session.target" ];
      wants = [ "graphical-session.target" ];
      after = [ "graphical-session.target" ];
      serviceConfig = {
        Type = "simple";
        ExecStart =
          "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
        Restart = "on-failure";
        RestartSec = 1;
        TimeoutStopSec = 10;
      };
    };
  };

  security.polkit.enable = true;
  services.fprintd.enable = true;
  security.pam.services.login.fprintAuth = true;
  security.pam.services.xscreensaver.fprintAuth = true;
  security.sudo-rs.enable = true;
  services.gnome.gnome-keyring.enable = true;
  security.pam.services.sddm.enableGnomeKeyring = true;
  security.pam.services.ollie.gnupg.enable = true;
  services.fwupd.enable = true;
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
  services.syncthing = let user = "ollie";
  in {
    enable = true;
    openDefaultPorts = true;
    user = "${user}";
    dataDir = "/home/${user}/.local/share/syncthing";
    configDir = "/home/${user}/.config/syncthing";
    guiAddress = "127.0.0.1:8384";
    overrideFolders = true;
    overrideDevices = true;

    settings.devices = {
      "Phone" = {
        id = "FBLIQCA-TQRYBUC-DOKDMM5-BFGUS7F-BQBXZ5N-L4XFL7S-HYN4I4K-T66HSQZ";
        autoAcceptFolders = true;
      };
    };
    settings.folders = {
      "Org" = {
        id = "csuap-tld6q";
        path = "/home/${user}/org/";
        devices = [ "Phone" ];
      };
      "Phone Images" = {
        id = "moto_g62_5g_rzur-photos";
        path = "/home/${user}/images/";
        devices = [ "Phone" ];
      };
      "Downloads" = {
        id = "cpdlo-g4olc";
        path = "/home/${user}/Downloads/";
        type = "receiveonly";
        devices = [ "Phone" ];
      };
    };
  };
  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  system.stateVersion = "23.11";
}
