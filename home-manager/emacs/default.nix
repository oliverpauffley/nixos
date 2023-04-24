{ config, lib, pkgs, ... }:

{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs-gtk;
    overrides = final: _prev: {
      nix-theme = final.callPackage ./theme.nix { inherit config; };
    };
    extraPackages = epkgs: with epkgs; [
      nix-theme
      vterm
    ];
    extraConfig = builtins.readFile ./init.el;
  };

  services.emacs =
    {
      enable = true;
      defaultEditor = true;
      startWithUserSession = true;
      socketActivation.enable = true;
    };
}
