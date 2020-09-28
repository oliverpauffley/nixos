{ pkgs, config, ... }:

{
  # Get greenclip for clipboard
  programs.rofi = {
    enable = true;
    theme = "gruvbox-dark-hard";
    terminal = "${pkgs.alacritty}/bin/alacritty";
  };
}
