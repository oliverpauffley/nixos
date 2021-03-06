{ config, pkgs, lib, ... }:

with pkgs;
let
  default-python = python3.withPackages (python-packages: with python-packages; [
    # basics
    pip
    pywal
    black
    setuptools
    wheel
    flake8
    virtualenv
    mypy
  ]);

in
{
  imports = [ ./options.nix ./mail.nix ./neovim.nix ./i3.nix ./dunst.nix ./polybar.nix ./rofi.nix ./alacritty.nix ./compton.nix ];
  nixpkgs.config.allowUnfree = true;

  home.keyboard.layout = "gb";

  home.packages = with pkgs; [
    # TERMINAL
    gotop
    tree
    gnupg
    feh
    yank
    # EMAIL
    mu
    openssl
    gnutls
    vdirsyncer
    khard
    # DEVELOPMENT
    postman
    default-python
    gcc
    gnumake
    go
    nodejs
    nixpkgs-fmt
    # OFFICE
    zathura
    # DEFAULT
    vlc
    spotify
    blueman
    slack
    haskellPackages.greenclip
    firefox
    # PASSWORDS
    _1password-gui
    _1password
    # NOTIFICATIONS
    libnotify
  ];


  programs = {
    home-manager.enable = true;
    command-not-found.enable = true;
    firefox = {
      enable = true;
    };
    bat = {
      enable = true;
      config = { theme = "zenburn"; };
    };
    git = {
      enable = true;
      userName = "oliverpauffley";
      userEmail = "mrpauffley@gmail.com";
      signing = {
        signByDefault = true;
        key = "EOBFCBFE7ED19B38";
      };
    };
    zsh = {
      enable = true;
      shellAliases =
        {
          vim = "nvim";
          cat = "bat";
        };
      oh-my-zsh.enable = true;
      oh-my-zsh.plugins = [ "git" "python" ];
    };
    starship = {
      enable = true;
      enableZshIntegration = true;
    };
  };


  xsession = {
    enable = true;
  };
}
