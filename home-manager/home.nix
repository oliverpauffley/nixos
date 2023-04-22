# This is your home-manager configuration file
# Use this to configure your home environment (it replaces ~/.config/nixpkgs/home.nix)

{ inputs, outputs, lib, config, pkgs, ... }: {
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
    inputs.nix-doom-emacs.hmModule
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

  home.packages = with pkgs;
    [
      (pkgs.nerdfonts.override { fonts = [ "Mononoki" "DroidSansMono" ]; })
      steam
      slack
      _1password-gui
      jq
      gnuplot
      fd
      ispell
      ncspot
      direnv
      ripgrep

      # keyboards
      wally-cli

      # c
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
      # rustup
      # rust-analyzer
      # editorconfig-core-c

      # go
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

  # a better direnv
  services.lorri.enable = true;

  programs.home-manager.enable = true;

  programs.doom-emacs = {
    enable = true;
    doomPrivateDir = ./doom.d; # Directory containing your config.el, init.el
    extraPackages = epkgs: [ epkgs.vterm ];
  };

  # Nicely reload system units when changing configs
  systemd.user.startServices = "sd-switch";

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  home.stateVersion = "22.11";
}
