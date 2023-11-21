{ config, lib, pkgs, ... }: {
  programs.emacs = {
    enable = true;
    package = pkgs.emacs-unstable;
    extraPackages = epkgs: with epkgs; [ vterm pdf-tools ];
  };

  services.emacs = {
    enable = true;
    defaultEditor = true;
    startWithUserSession = "graphical";
    socketActivation.enable = true;
  };
}
