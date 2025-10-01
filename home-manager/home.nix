# This is your home-manager configuration file
# Use this to configure your home environment (it replaces ~/.config/nixpkgs/home.nix)
{ inputs, outputs, lib, config, pkgs, rust-overlay, nix-colors, ... }: {
  # You can import other home-manager modules here
  imports = [
    # If you want to use modules your own flake exports (from modules/home-manager):
    outputs.homeManagerModules.fonts

    # Or modules exported from other flakes (such as nix-colors):
    # inputs.nix-colors.homeManagerModules.default
    inputs.nix-colors.homeManagerModules.default

    # You can also split up your configuration and import pieces of it here:
    ./git.nix
    ./kitty.nix
    ./emacs
    ./fish.nix
    ./rofi.nix
    ./rust.nix
    ./starship.nix
    ./xmonad
    ./autorandr.nix
    ./email.nix
    ./k9s.nix
    ./haskell.nix
    ./sops.nix
  ];
  programs.info.enable = true;

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
      #
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
  colorScheme = nix-colors.colorSchemes.dracula;
  fontProfiles = {
    enable = true;
    monospace = {
      family = "Departure Mono";
      package = pkgs.departure-mono;
    };
    regular = {
      family = "Mononoki Nerd Font";
      package = pkgs.nerd-fonts.mononoki;
    };
  };

  services.random-background = {
    enable = true;
    display = "fill";
    imageDirectory = "%h/wallpapers";
    interval = "1h";
  };

  home.packages = with pkgs; [
    emacs-all-the-icons-fonts
    chromium
    jq
    rq
    fx
    gnuplot
    fd
    ispell
    (aspellWithDicts (dicts: with dicts; [ en en-computers en-science ]))
    direnv
    ripgrep
    eza
    bat
    pandoc
    util-linux
    xan
    postgresql
    xclip
    yaml-language-server
    dive # docker image viewer
    procs # replacement for ps
    imagemagick
    hyperfine # measure cli speed
    du-dust # see hard disk usage
    ffmpeg
    python311Packages.pygments # syntax highlighter
    zathura # pdf viewer
    pkg-config
    openssl
    openssl.dev
    terraform
    shfmt
    eclint
    dnsutils
    unstable.just
    sqlite
    sqlitecpp
    wordnet
    exercism
    mermaid-cli # diagrams from code
    nodePackages_latest.prettier # code formatting
    vlc
    unrar
    comma # run commands that aren't installed with ","
    (import ./git_visualizer.nix { inherit pkgs; })
    (import ./go_coverage.nix { inherit pkgs; })
    wineWowPackages.stable
    dhall
    dhallPackages.dhall-kubernetes
    multi-gitter
    btop
    unstable.slack
    mariadb
    calibre # ebooks
    gomerge
    remmina
    age
    sops
    comby
    mergiraf
    semgrep
    yad # display keybindings with a dialog box
    xdotool # do stuff with xmonad via cli
    nh # nix-helper
    localsend # send files to nearby devices

    # keyboards
    wally-cli

    # common lisp
    sbcl

    # Scheme
    racket

    # Theorem Provers
    (agda.withPackages [ agdaPackages.standard-library ])

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
    nixfmt-classic
    nil
    statix

    # go
    unstable.go
    go-outline
    gocode-gomod
    godef
    golint
    gomodifytags
    gopkgs
    unstable.gopls
    gore
    gotests
    gotestsum
    unstable.golangci-lint
    gotools
    unstable.moq
    sqlc
    unstable.structurizr-cli
    graphviz

    # kube
    kubectl
    kustomize

    # json/grpc
    evans
    grpcurl
    unstable.postman

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

    # rust
    rust-analyzer

    # Janet
    janet
    jpm

    # Painting
    krita

    # python
    python3
    black
    pyright
    python311Packages.pyflakes
    pipenv
    python311Packages.pytest

    # irc
    gnutls

    emacs-all-the-icons-fonts

    # run things with fhs to get a shell that works for most linux binaries
    (let base = pkgs.appimageTools.defaultFhsEnvArgs;
    in pkgs.buildFHSEnv (base // {
      name = "fhs";
      targetPkgs = pkgs:
        # pkgs.buildFHSUserEnv provides only a minimal FHS environment,
        # lacking many basic packages needed by most software.
        # Therefore, we need to add them manually.
        #
        # pkgs.appimageTools provides basic packages required by most software.
        (base.targetPkgs pkgs) ++ (with pkgs; [
          pkg-config
          ncurses
          # Feel free to add more packages here if needed.
        ]);
      profile = "export FHS=1";
      runScript = "bash";
      extraOutputsToInstall = [ "dev" ];
    }))
  ];

  programs.vim = {
    enable = true;
    defaultEditor = true;
  };

  services.picom.enable = true;
  gtk = {
    enable = true;
    font.name = "${config.fontProfiles.regular.family} 12";
    theme = {
      name = "Dracula";
      package = pkgs.dracula-theme;
    };
  };
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  # default programs
  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "application/pdf" = [ "zathura.desktop" "firefox.desktop" ];
      "image/png" = [ "feh.desktop" ];
      "text/plain" = [ "emacs.desktop" ];
      "x-scheme-handler/http" = [ "firefox.desktop" ];
      "x-scheme-handler/https" = [ "firefox.desktop" ];
      "x-scheme-handler/chrome" = [ "firefox.desktop" ];
      "text/html" = [ "firefox.desktop" ];
      "application/x-extension-htm" = [ "firefox.desktop" ];
      "application/x-extension-html" = [ "firefox.desktop" ];
      "application/x-extension-shtml" = [ "firefox.desktop" ];
      "application/xhtml+xml" = [ "firefox.desktop" ];
      "application/x-extension-xhtml" = [ "firefox.desktop" ];
      "application/x-extension-xht" = [ "firefox.desktop" ];
    };
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
  home.stateVersion = "25.05";
}
