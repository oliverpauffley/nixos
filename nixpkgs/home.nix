{ config, pkgs, lib, ... }:

with pkgs;
let
  default-python = python3.withPackages (python-packages: with python-packages; [
    (callPackage ./pylibs/binancepy.nix { }) (callPackage ./pylibs/bit.nix { })
    # basics
    pip pywal black setuptools wheel flake8 virtualenv mypy
  ]);

in
  {
    imports = [ ./i3.nix ./polybar.nix ./rofi.nix ./alacritty.nix ./compton.nix ];
    nixpkgs.config.allowUnfree = true;

    home.keyboard.layout = "gb";


    home.packages = with pkgs; [
      # TERMINAL
      gotop tree gnupg 
      feh
      # DEVELOPMENT
      postman
      default-python gcc gnumake go
      # OFFICE
      zathura 
      # DEFAULT
      vlc spotify blueman  firefox slack 
      # PASSWORDS
      _1password-gui _1password 
    ];


    programs = {
      home-manager.enable = true;
      command-not-found.enable = true;
      neovim = {
        enable = true;
        vimAlias = true;
        extraConfig = 
        ''
          let mapleader = "\\"
          colorscheme gruvbox
          let g:gruvbox_contrast_dark = 1
        '';
        plugins = with pkgs.vimPlugins; [
          vim-nix 
          gruvbox
        ];
      }; 
      bat = {
        enable = true;
        config = { theme = "zenburn";};
      };
      git = {
        enable = true;
        userName = "oliverpauffley";
        userEmail = "mrpauffley@gmail.com";
      };
      zsh = {
        enable = true;
        shellAliases =
          {
            vim = "nvim";
            cat = "bat";
          };
          oh-my-zsh.enable = true;
          oh-my-zsh.plugins = ["git" "python"];
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
