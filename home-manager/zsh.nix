{ config, lib, pkgs, ... }:

{
  programs.zsh = {
    enable = true;
    autocd = true;
    dotDir = ".config/zsh";
    enableAutosuggestions = true;
    enableCompletion = true;
    shellAliases = {
      k = "kubectl";
      update =
        "sudo nixos-rebuild -I nixos-config=/home/ollie/.dotfiles/configuration.nix switch";
      update-home =
        "home-manager switch -f /home/ollie/.dotfiles/home-manager/home.nix";
    };

    oh-my-zsh = {
      enable = true;
      plugins = [ "git" ];
      theme = "simple";
    };
  };

}
