{ config, pkgs, lib, ... }: {
  programs.home-manager.enable = true;
  imports = [ ./zsh.nix ./rofi.nix ./git.nix ];

  nixpkgs.config.packageOverrides = pkgs: {
    nur = import <nur> { inherit pkgs; };
  };

  # run emacs with extra packages
  programs.emacs = {
    enable = true;
    extraPackages = epkgs: [ epkgs.vterm ];
  };

  # gpg
  programs.gpg.enable = true;

  # spotifyd
  services.spotifyd = {
    enable = true;
    settings = {
      global = {
        username = "mrpauffley@gmail.com";
        password_cmd = "${pkgs.pass}/bin/pass spotify";

        device_name = "spotifyd";
        device_type = "computer";
      };
    };
  };

  # firefox
  programs.firefox = {
    enable = true;
    extensions = with pkgs.nur.repos.rycee.firefox-addons;
      [ onepassword-password-manager ];
    profiles."Ollie" = {
      isDefault = true;
      settings = {
        "browser.startup.homepage" = "https://nixos.org";
        "browser.search.region" = "GB";
      };
    };
  };

  # Go
  programs.go.enable = true;
  home.sessionVariables = {
    GOPRIVATE = "github.com/utilitywarehouse/*";
  };

  home.packages = with pkgs;
    [
      slack
      _1password-gui
      jq
      terraform
      gnuplot
      fd
      ispell
      spotify-tui
      direnv

      # keyboards
      wally-cli

      # games
      cataclysm-dda

      # c
      cmake
      shellcheck
      coreutils
      clang
      cmake
      clang-tools

      # nix
      ripgrep
      nixfmt
      rnix-lsp

      # rust
      rustup
      rust-analyzer
      editorconfig-core-c

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

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "ollie";
  home.homeDirectory = "/home/ollie";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  #home.stateVersion = "21.05";
}
