{ config, lib, pkgs, nix-colors, ... }: {
  programs.rofi = {
    enable = true;
    terminal = "\${pkgs.alacritty}/bin/alacritty";
  };
}
