{ config, lib, pkgs, ... }: {
  programs.emacs = {
    enable = true;
    package = pkgs.emacs-unstable;
    extraPackages = epkgs: with epkgs; [ vterm ];
  };

  services.emacs = {
    enable = true;
    defaultEditor = true;
    client.enable = true;
    client.arguments = [ "-nc" ];
    startWithUserSession = "graphical";
    socketActivation.enable = true;
  };
}
