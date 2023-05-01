{ config, lib, pkgs, nix-colors, ... }: {
  programs.rofi = {
    enable = true;
    terminal = "\${pkgs.wezterm}/bin/wezterm";
  };
}
