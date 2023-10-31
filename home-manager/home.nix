# This is your home-manager configuration file
# Use this to configure your home environment (it replaces ~/.config/nixpkgs/home.nix)
{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
  rust-overlay,
  nix-colors,
  ...
}: {
  # You can import other home-manager modules here
  imports = [
    # If you want to use modules your own flake exports (from modules/home-manager):
    outputs.homeManagerModules.fonts

    # Or modules exported from other flakes (such as nix-colors):
    # inputs.nix-colors.homeManagerModules.default
    inputs.nix-colors.homeManagerModules.default

    # You can also split up your configuration and import pieces of it here:
    ./git.nix
    ./alacritty.nix
    ./emacs
    ./fish.nix
    ./nushell.nix
    ./rofi.nix
    ./rust.nix
    ./i3
    ./autorandr.nix
    ./email.nix
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
      allowUnfreePredicate = _: true;
    };
  };

  home = {
    username = "ollie";
    homeDirectory = "/home/ollie";
  };
  # fonts
  fonts.fontconfig.enable = true;

  # colors
  colorScheme = nix-colors.colorSchemes.tokyo-night-dark;
  fontProfiles = {
    enable = true;
    monospace = {
      family = "GohuFont 14 Nerd Font";
      package = pkgs.nerdfonts.override {fonts = ["Gohu"];};
    };
    regular = {
      family = "mononoki Nerd Font";
      package = pkgs.nerdfonts.override {fonts = ["Mononoki"];};
    };
  };

  home.packages = with pkgs; [
    emacs-all-the-icons-fonts
    unstable.slack
    jq
    gnuplot
    fd
    ispell
    (aspellWithDicts (dicts: with dicts; [en en-computers en-science]))
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
    xclip
    yaml-language-server
    pkg-config
    dive # docker image viewer
    procs # replacement for ps
    imagemagick
    hyperfine # measure cli speed
    du-dust # see hard disk usage
    beancount
    beancount-language-server
    fava
    ffmpeg
    python311Packages.pygments # syntax highlighter
    zathura # pdf viewer

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
    alejandra
    statix

    # go
    unstable.go_1_21
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
    gotestsum

    # kube
    kubectl
    k9s
    kustomize

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

    # rust
    rust-analyzer

    # Ruby
    ruby

    # Janet
    janet
    jpm

    # haskell
    unstable.stack
    unstable.cabal-install
    unstable.ghc
    unstable.haskell-language-server

    # Painting
    krita

    # python
    python312
    black
    pyright

    # Games
    cataclysm-dda

    # irc
    gnutls

    emacs-all-the-icons-fonts
  ];

  services.picom.enable = true;

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
