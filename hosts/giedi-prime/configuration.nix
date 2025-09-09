# This is your system's configuration file.
# Use this to configure your system environment (it replaces /etc/nixos/configuration.nix)
{ inputs, outputs, lib, config, pkgs, ... }: {
  # You can import other NixOS modules here
  imports = [
    # If you want to use modules your own flake exports (from modules/nixos):
    # outputs.nixosModules.example

    # Or modules from other flakes (such as nixos-hardware):
    # inputs.hardware.nixosModules.common-cpu-intel

    # You can also split up your configuration and import pieces of it here:
    # ./users.nix

    # Import your generated (nixos-generate-config) hardware configuration
    ./hardware-configuration.nix

    ../../services/wiresteward
    ../common/sops.nix
    ../common/users.nix
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

    envVars = {
      NIX_GITHUB_PRIVATE_USERNAME = config.sops.secrets.github_username.path;
      NIX_GITHUB_PRIVATE_PASSWORD = config.sops.secrets.github_token.path;
    };

    settings = {
      # Enable flakes and new 'nix' command
      experimental-features = "nix-command flakes";
      # Deduplicate and optimize nix store
      auto-optimise-store = true;
      trusted-public-keys =
        [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
      substituters = [ "https://cache.iog.io" ];
      trusted-users = [ "ollie" "root" ];
    };
  };

  networking.hostName = "giedi-prime";
  networking.networkmanager.enable = true;
  networking.nameservers = [ "1.1.1.1" "9.9.9.9" "8.8.8.8" ];
  networking.networkmanager.dns = "none";
  networking.dhcpcd.extraConfig = "nohook resolv.conf";
  services.resolved.enable = false;

  programs.nm-applet.enable = true;

  # Bootloader
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  users.mutableUsers = false;
  users.users = {
    ollie = {
      isNormalUser = true;
      hashedPasswordFile = config.sops.secrets.ollie_passwd.path;
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
    openssl
  ];
  environment.sessionVariables = {
    XDG_CACHE_HOME = "$HOME/.cache";
    XDG_CONFIG_HOME = "$HOME/.config";
    XDG_DATA_HOME = "$HOME/.local/share";
    XDG_STATE_HOME = "$HOME/.local/state";
  };
  environment.variables = {
    PKG_CONFIG_PATH = "${pkgs.openssl.dev}/lib/pkgconfig";
  };
  fonts.packages = with pkgs; [
    nerd-fonts.mononoki
    nerd-fonts.gohufont
    nerd-fonts.droid-sans-mono
    departure-mono
    luculent
  ];
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
  services.pipewire.wireplumber.enable = true;

  # Enable zsa keyboards
  hardware.keyboard.zsa.enable = true;

  # tablet
  hardware.opentabletdriver = {
    enable = true;
    daemon.enable = true;
  };

  # patch downloaded binaries
  programs.nix-ld.enable = true;

  # Sets up all the libraries to load
  programs.nix-ld.libraries = with pkgs; [
    stdenv.cc.cc
    zlib
    fuse3
    icu
    nss
    openssl
    curl
    expat
    # ...
  ];
  # Enable the X11 windowing system.
  services.libinput = {
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
  hardware.graphics.enable = true;
  services.xserver = {
    enable = true;
    xkb = {
      layout = "gb";
      options = "caps:ctrl_modifier";
    };
    dpi = 180;
    upscaleDefaultCursor = true;
    videoDrivers = [ "displaylink" "modesetting" ];
    displayManager.gdm.enable = true;
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };

    desktopManager = { xterm.enable = false; };

  };
  services.dbus.enable = true;

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
  security.sudo-rs.enable = true;
  services.gnome.gnome-keyring.enable = true;
  security.pam.services.sddm.enableGnomeKeyring = true;
  security.pam.services.ollie.gnupg.enable = true;
  security.pam.services.ollie.gnupg.storeOnly = true;
  services.fwupd.enable = true;
  services.pcscd.enable = true;

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  # printing
  services.printing.enable = true;
  services.printing.drivers = [
    pkgs.gutenprint
    pkgs.gutenprintBin
    pkgs.fxlinuxprint
    pkgs.foomatic-db-ppds-withNonfreeDb
  ];

  # power managment
  services.upower.enable = true;

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
    overrideFolders = false;
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

  networking.firewall.allowedTCPPorts = [ 8083 ];

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  system = {
    stateVersion = "23.11";
    activationScripts.diff = ''
          if [[ -e //run/current/system ]]; then
            ${pkgs.nix}/bin/nix store diff-closures /run/current-system "$systemConfig"
      fi
    '';
  };
}
