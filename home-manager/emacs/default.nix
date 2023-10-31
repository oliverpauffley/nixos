{
  config,
  lib,
  pkgs,
  ...
}: {
  programs.emacs = {
    enable = true;
    package = pkgs.emacs-gtk;
    extraPackages = epkgs: with epkgs; [vterm];
  };

  services.emacs = {
    enable = true;
    defaultEditor = true;
    startWithUserSession = "graphical";
    socketActivation.enable = true;
  };
}
