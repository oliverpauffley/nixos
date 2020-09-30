# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [
      # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;

  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/sda"; # or "nodev" for efi only

  networking = {
    hostName = "nixos"; # Define your hostname.
    networkmanager.enable = true;
    # The global useDHCP flag is deprecated, therefore explicitly set to false here.
    # Per-interface useDHCP will be mandatory in the future, so this generated config
    # replicates the default behaviour.
    useDHCP = false;
    interfaces.ens18.useDHCP = true;
  };

  # Autoupdate from unstable
  system.autoUpgrade.channel = "https://nixos.org/channels/nixos-unstable";
  system.autoUpgrade.enable = true;

  # Add nur packages
  nixpkgs.config.pkgOverrides = pkgs: {
    nur = import (builtins.fetchTarball "https://github.com/nix-community/NUR/archive/master.tar.gz") {
      inherit pkgs;
    };
  };

  # TODO Keep only the 10 most recent generations


  # Select internationalisation properties.
  i18n.defaultLocale = "en_GB.UTF-8";
  i18n.extraLocaleSettings = {
    LC_ALL = "en_GB.UTF-8";
    LANG = "en_GB.UTF-8";
  };
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  # Set your time zone.
  time.timeZone = "Europe/London";

  # Use unfree packages
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  environment.systemPackages = with pkgs; [
    vim
    vimHugeX
    # passwords
    gnupg
    gnome3.gnome-keyring
    libsecret
    # general
    spotify
    blueman
    keybase
    keybase-gui
    font-manager
    home-manager
    # development
    git
    gitkraken
    postman
    # terminal
    wget
    nnn
    neofetch
    zip
    unzip
    unrar
    jq
  ];
  environment.variables.EDITOR = "vim";

  # Setup fonts
  fonts.fonts = with pkgs; [
    powerline-fonts
    font-awesome
    mononoki
  ];

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable sound.
  services.greenclip.enable = true;

  # Enable keybase.
  services.keybase.enable = true;
  services.kbfs.enable = true;

  # Enable autorandr to manage plugins
  services.autorandr.enable = true;

  # x server settings.
  services.xserver = {
    # Enable the X11 windowing system.
    enable = true;
    layout = "gb";

    # Enable touchpad support.
    libinput.enable = true;

    # Enable the Desktop Environment.
    displayManager.defaultSession = "none+i3";
    desktopManager.xterm.enable = false;
    displayManager.lightdm.enable = true;
    windowManager.i3.enable = true;
    windowManager.i3.package = pkgs.i3-gaps;

    # set resolution 
    resolutions = [{ x = 1600; y = 1200; }];

  };
  # Enable gnome keyring.
  services.gnome3.gnome-keyring.enable = true;
  programs.seahorse.enable = true;

  security.pam.services.login = {
    setEnvironment = true;
    setLoginUid = true;
    startSession = true;
    unixAuth = true;
    updateWtmp = true;

    enableGnomeKeyring = true;
  };


  # start docker
  virtualisation.docker.enable = true;


  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.ollie = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "docker" ]; # Enable corret user groups
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.03"; # Did you read the comment?

}
