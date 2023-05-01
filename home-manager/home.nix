# This is your home-manager configuration file
# Use this to configure your home environment (it replaces ~/.config/nixpkgs/home.nix)

{ inputs, outputs, lib, config, pkgs, rust-overlay, ... }: {
  # You can import other home-manager modules here
  imports = [
    # If you want to use modules your own flake exports (from modules/home-manager):
    # outputs.homeManagerModules.example

    # Or modules exported from other flakes (such as nix-colors):
    # inputs.nix-colors.homeManagerModules.default

    # You can also split up your configuration and import pieces of it here:
    # ./nvim.nix
    ./git.nix
    ./wezterm.nix
    ./i3.nix
    ./polybar.nix
    ./picom.nix
    #./xmonad.nix
    ./emacs
    ./fish.nix
    ./rofi.nix

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
    arandr
    exa
    postman
    buf

    # keyboards
    wally-cli

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
    gotools

    # kube
    kubectl
    k9s

    # json/grpc
    evans
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
