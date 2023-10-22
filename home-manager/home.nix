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

    # Games
    cataclysm-dda

    # irc
    gnutls

    emacs-all-the-icons-fonts
  ];

  services.picom.enable = true;

  programs.autorandr = {
    enable = true;
    profiles = {
      "work" = {
        fingerprint = {
          HDMI-1-0 = "00ffffffffffff001e6d805bb42d0000041f0103803c2278ea8cb5af4f43ab260e5054254b007140818081c0a9c0b300d1c08100d1cf5aa000a0a0a0465030203a0055502100001a000000fd0030781ee63c000a202020202020000000fc004c4720554c545241474541520a000000ff003130344e54514430423730300a015a020344f1230907074d100403011f13123f5d5e5f60616d030c001000b83c20006001020367d85dc401788003e30f0018681a00000101307800e305c000e6060501605928d97600a0a0a0345030203a0055502100001a565e00a0a0a029503020350055502100001a000000000000000000000000000000000000000000000027";
          eDP-1 = "00ffffffffffff0009e5ec0800000000011d0104b523137802df50a35435b5260f50540000000101010101010101010101010101010152d000a0f0703e803020350058c21000001a00000000000000000000000000000000001a000000fe00424f452048460a202020202020000000fe004e4531353651554d2d4e36410a017702030f00e3058000e6060501737321000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008b";
        };
        config = {
          HDMI-1-0 = {
            dpi = 100;
            enable = true;
            mode = "2560x1440";
            position = "2560x0";
            primary = true;
            rate = "99.95";
          };
          eDP-1 = {
            dpi = 180;
            enable = true;
            mode = "2560x1440";
            position = "0x0";
            rate = "60.00";
          };
        };
      };
      "default" = {
        fingerprint = {
          eDP-1 = "00ffffffffffff0009e5ec0800000000011d0104b523137802df50a35435b5260f50540000000101010101010101010101010101010152d000a0f0703e803020350058c21000001a00000000000000000000000000000000001a000000fe00424f452048460a202020202020000000fe004e4531353651554d2d4e36410a017702030f00e3058000e6060501737321000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008b";
        };
        config = {
          eDP-1 = {
            dpi = 180;
            enable = true;
            mode = "2560x1440";
            primary = true;
            position = "0x0";
            rate = "60.00";
          };
        };
      };
      "tosh" = {
        fingerprint = {
          HDMI-1-0 = "00ffffffffffff0010ac7240533232312c17010380331d78ea6ea5a3544f9f26115054a54b00714f8180d1c001010101010101010101023a801871382d40582c4500fe1f1100001e000000ff004b46383759334153313232530a000000fc0044454c4c205532333132484d0a000000fd00384c1e5311000a2020202020200042";
          eDP-1 = "00ffffffffffff0009e5ec0800000000011d0104b523137802df50a35435b5260f50540000000101010101010101010101010101010152d000a0f0703e803020350058c21000001a00000000000000000000000000000000001a000000fe00424f452048460a202020202020000000fe004e4531353651554d2d4e36410a017702030f00e3058000e6060501737321000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008b";
        };
        config = {
          eDP-1 = {
            dpi = 180;
            enable = true;
            mode = "2560x1440";
            primary = true;
            position = "1920x0";
            rate = "60.00";
          };
          HDMI-1-0 = {
            dpi = 100;
            enable = true;
            mode = "1920x1080";
            position = "0x0";
            rate = "99.95";
          };
        };
      };
    };
    hooks = {
      postswitch = {
        "notify-i3" = "${pkgs.i3}/bin/i3-msg restart";
        "change-dpi" = ''
          case "$AUTORANDR_CURRENT_PROFILE" in
            default)
              DPI=180
              ;;
            home)
              DPI=180
              ;;
            work)
              DPI=180
              ;;
            tosh)
              DPI=180
              ;;
            *)
              echo "Unknown profile: $AUTORANDR_CURRENT_PROFILE"
              exit 1
          esac

          echo "Xft.dpi: $DPI" | ${pkgs.xorg.xrdb}/bin/xrdb -merge
        '';
      };
    };
  };

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
