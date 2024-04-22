{ config, lib, pkgs, ... }: {
  programs.fish = {
    enable = true;
    shellAbbrs = {
      hms = "home-manager switch --flake .#";
      nos = "sudo nixos-rebuild switch --flake .#";
    };
    shellAliases = {
      ls = "eza";
      cat = "bat";
      giff-account-id = "uuidgen -n 01d6ade7-f2eb-5e7d-b36d-9468f7bae3fb -s -N";
      cd = "z";
    };
    interactiveShellInit = ''

      # Disable greeting
      set fish_greeting

      # Work go export and github settings
      set -Ux GOPRIVATE "github.com/utilitywarehouse/*"

      # Cargo use git to fetch
      set -Ux CARGO_NET_GIT_FETCH_WITH_CLI true

      # suppress direnv logging
      set -gx DIRENV_LOG_FORMAT ""

      set -U fish_user_paths $fish_user_paths $HOME/.config/emacs/bin
    '';
    functions = { ec = { body = "emacsclient --create-frame $argv &"; }; };
    plugins = [
      # Enable a plugin (here grc for colorized command output) from nixpkgs
      #{ name = "grc"; src = pkgs.fishPlugins.grc.src; }
    ];
  };
  services.gpg-agent.enableFishIntegration = true;
  programs.zoxide = {
    enable = true;
    enableFishIntegration = true;
  };
}
