# This is your home-manager configuration file
# Use this to configure your home environment (it replaces ~/.config/nixpkgs/home.nix)

{ inputs, outputs, lib, config, pkgs, rust-overlay, ... }:

let nix-colors-lib = inputs.nix-colors.lib.contrib { inherit pkgs; };
in
{
  # You can import other home-manager modules here
  imports = [
    # If you want to use modules your own flake exports (from modules/home-manager):
    outputs.homeManagerModules.fonts

    # Or modules exported from other flakes (such as nix-colors):
    # inputs.nix-colors.homeManagerModules.default
    inputs.hyprland.homeManagerModules.default
    inputs.nix-colors.homeManagerModules.default

    # You can also split up your configuration and import pieces of it here:
    ./git.nix
    ./alacritty.nix
    ./emacs
    ./fish.nix
    ./rofi.nix
    ./hyprland
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
      # Workaround for https://github.com/nix-community/home-manager/issues/2942
      allowUnfreePredicate = (_: true);
    };
  };

  home = {
    username = "ollie";
    homeDirectory = "/home/ollie";
  };
  # fonts
  fonts.fontconfig.enable = true;

  # colors
  colorScheme = nix-colors-lib.colorSchemeFromPicture {
    path = ../wallpapers/wallpaper.jpg;
    kind = "dark";
  };

  fontProfiles = {
    enable = true;
    monospace = {
      family = "mononoki Nerd Font";
      package = pkgs.nerdfonts.override { fonts = [ "Mononoki" ]; };
    };
    regular = {
      family = "mononoki Nerd Font";
      package = pkgs.nerdfonts.override { fonts = [ "Mononoki" ]; };
    };
  };
  home.packages = with pkgs; [
    (pkgs.nerdfonts.override { fonts = [ "Mononoki" "DroidSansMono" "Gohu" ]; })
    emacs-all-the-icons-fonts
    slack
    _1password-gui
    jq
    gnuplot
    fd
    ispell
    (aspellWithDicts (dicts: with dicts; [ en en-computers en-science ]))
    ncspot
    direnv
    ripgrep
    exa
    bat
    postman
    pandoc
    util-linux
    xsv
    postgresql

    # keyboards
    wally-cli

    # common lisp
    sbcl

    # c
    libtool
    gnumake
    cmake
    shellcheck
    coreutils
    clang
    cmake
    clang-tools

    # nix
    nixfmt
    rnix-lsp

    # rust
    rustup
    rust-analyzer

    # go
    go
    go-outline
    gocode
    gocode-gomod
    godef
    golint
    gomodifytags
    gopkgs
    gopls
    gore
    gotests

    # kube
    kubectl
    k9s

    # json/grpc
    evans

    # debugger dependencies
    nodejs
    lldb

    # latex
    texlive.combined.scheme-full

    # clojure
    clojure
    clj-kondo
    clojure-lsp
    leiningen
    neil
    jet
    openjdk8

    # Open ssl
    openssl
    openssl.dev
    pkg-config

    # Ruby
    ruby

    # Janet
    janet
    jpm
  ];

  # a better direnv with fish integration
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  # auto mount removable disks
  services.udiskie = {
    enable = true;
    tray = "always";
  };
  programs.home-manager.enable = true;

  # Nicely reload system units when changing configs
  systemd.user.startServices = "sd-switch";

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  home.stateVersion = "22.11";
}
