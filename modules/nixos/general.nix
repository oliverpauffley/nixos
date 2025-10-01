{ config, inputs, outputs, lib, pkgs, ... }: {
  imports = [ ./users/default.nix ];
  nixpkgs = {
    overlays = [
      outputs.overlays.additions
      outputs.overlays.modifications
      outputs.overlays.unstable-packages
    ];
    config = {
      allowUnfree = true;
      nvidia.acceptLicense = true;
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

  networking.networkmanager.enable = true;
  networking.nameservers = [ "1.1.1.1" "9.9.9.9" "8.8.8.8" ];
  networking.networkmanager.dns = "none";
  networking.dhcpcd.extraConfig = "nohook resolv.conf";

  services.resolved.enable = false;
  programs.nm-applet.enable = true;

  users = {
    mutableUsers = false;
    users = {
      ollie = {
        isNormalUser = true;
        hashedPasswordFile = config.sops.secrets.ollie_passwd.path;
        extraGroups = [ "wheel" "docker" "networkmanager" "audio" ];
        shell = pkgs.fish;
      };
    };
  };
  # really really set the shell
  environment.shells = [ pkgs.fish ];

  # 1password setup
  programs._1password.enable = true;

  services.openssh = {
    enable = true;
    settings.PermitRootLogin = "no";
  };

  programs.fish.enable = true;
  environment.systemPackages = with pkgs; [
    dict
    vim
    gnupg
    pinentry-all
    gtk3
    pavucontrol
    unzip
    polkit_gnome
    mu
    openssl
  ];
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

  services.dbus.enable = true;

  security.polkit.enable = true;
  security.sudo-rs.enable = true;
  security.pam.services.ollie.gnupg.enable = true;
  security.pam.services.ollie.gnupg.storeOnly = true;
  services.fwupd.enable = true;
  services.pcscd.enable = true;

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  # docker
  virtualisation.docker.enable = true;

  # Enable zsa keyboards
  hardware.keyboard.zsa.enable = true;
}
