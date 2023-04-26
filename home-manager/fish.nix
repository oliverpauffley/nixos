{ config, lib, pkgs, ... }: {
  programs.fish = {
    enable = true;
    shellAbbrs = {
      hms = "home-manager switch --flake .#";
      nos = "sudo nixos-rebuild switch --flake .#";
    };
    interactiveShellInit = ''
      set fish_greeting # Disable greeting
      set -Ux GOPRIVATE "github.com/utilitywarehouse/*"
    '';
    functions = { ec = { body = "emacsclient --create-frame $argv &"; }; };
    plugins = [
      # Enable a plugin (here grc for colorized command output) from nixpkgs
      #{ name = "grc"; src = pkgs.fishPlugins.grc.src; }
    ];
  };
  services.gpg-agent.enableFishIntegration = true;
}
