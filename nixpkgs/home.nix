{ config, pkgs, lib, ... }:

with pkgs;
let
  default-python = python3.withPackages (python-packages: with python-packages; [
    (callPackage ./pylibs/binancepy.nix { }) (callPackage ./pylibs/bit.nix { })
    # basics
    pip pywal black setuptools wheel twine flake8 virtualenv pudb mypy
    # utils
    aioconsole aiohttp
  ]);

in
  {
    imports = [ ./i3.nix ./polybar.nix ./rofi.nix ./alacritty.nix ./compton.nix ];
    nixpkgs.config.allowUnfree = true;

    home.keyboard.layout = "gb";


    home.packages = with pkgs; [
      # TERMINAL
      gotop zip unrar unzip xorg.xev escrotum tree gnupg 
      aria2 imagemagick feh httpie
      # DEVELOPMENT
      postman
      default-python gcc gnumake
      # OFFICE
      zathura 
      # DEFAULT
      vlc spotify blueman _1password
    ];


    programs = {
      home-manager.enable = true;
      command-not-found.enable = true;
      neovim = {
        enable = true;
        vimAlias = true;
        plugins = with pkgs.vimPlugins; [
          vim-nix
        ];
      }; 
      bat = {
        enable = true;
        config = { theme = "gruvbox";};
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

      services.gnome-keyring = {
        enable = true;
        components = ["ssh" "secrets"];
      };

      xsession.enable = true;
    }
