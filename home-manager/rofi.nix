{ config, lib, pkgs, nix-colors, ... }: {
  programs.rofi = {
    enable = true;
    terminal = "\${pkgs.wezterm}/bin/wezterm";
    font = "GohuFont Nerd Font 15";
    theme = let
      colors = config.colorScheme.colors;
      inherit (config.lib.formats.rasi) mkLiteral;
    in {

      "*" = {
        red = mkLiteral "#${colors.base08}";
        blue = mkLiteral "#${colors.base0D}";
        lightfg = mkLiteral "#${colors.base06}";
        lightbg = mkLiteral "#${colors.base01}";
        foreground = mkLiteral "#${colors.base05}";
        background = mkLiteral "#${colors.base01}";
        background-color = mkLiteral "#${colors.base01}";
        separatorcolor = mkLiteral "@foreground";
        border-color = mkLiteral "@foreground";
        selected-normal-foreground = mkLiteral "@lightbg";
        selected-normal-background = mkLiteral "@lightfg";
        selected-active-foreground = mkLiteral "@background";
        selected-active-background = mkLiteral "@blue";
        selected-urgent-foreground = mkLiteral "@background";
        selected-urgent-background = mkLiteral "@red";
        normal-foreground = mkLiteral "@foreground";
        normal-background = mkLiteral "@background";
        active-foreground = mkLiteral "@blue";
        active-background = mkLiteral "@background";
        urgent-foreground = mkLiteral "@red";
        urgent-background = mkLiteral "@background";
        alternate-normal-foreground = mkLiteral "@foreground";
        alternate-normal-background = mkLiteral "@lightbg";
        alternate-active-foreground = mkLiteral "@blue";
        alternate-active-background = mkLiteral "@lightbg";
        alternate-urgent-foreground = mkLiteral "@red";
        alternate-urgent-background = mkLiteral "@lightbg";
        spacing = 2;
      };
    };
  };
}
