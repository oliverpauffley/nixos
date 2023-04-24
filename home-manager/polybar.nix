{ config, lib, pkgs, nix-colors, ... }:
let colors = config.colorScheme.colors;
in
{
  services.polybar = {
    enable = true;
    package = pkgs.polybar.override {
      i3Support = true;
      alsaSupport = true;
      iwSupport = true;
      githubSupport = true;
    };
    config = {
      "bar/top" = {
        font-0 = "GohuFont Nerd Font:size=16";
        monitor = "eDP-1";
        width = "100%";
        height = "2%";
        radius = 0;
        background-1 = "#dd${colors.base00}";
        background-2 = "#dd${colors.base01}";
        background-3 = "#dd${colors.base02}";
        background-4 = "#dd${colors.base03}";
        background-5 = "#dd${colors.base04}";
        background-6 = "#dd${colors.base05}";
        background = "#dd${colors.base00}";
        foreground = "#ff${colors.base07}";

        modules-right = "date";
        modules-left = "i3";
      };
      "module/date" = {
        type = "internal/date";
        internal = 5;
        date = "%Y-%m-%d";
        time = "%H:%M";
        label = "%date% %time%";
      };
      "module/i3" = {
        type = "internal/i3";
        scroll-up = "i3wm-wsnext";

        label-focused = "%index%";
        label-focused-foreground = "#${colors.base08}";
        label-focused-background = "#${colors.base01}";
        label-focused-underline = "#${colors.base08}";
        label-focused-padding = 1;

        label-unfocused = "%index%";
        label-unfocused-foreground = "#${colors.base06}";
        label-unfocused-background = "#${colors.base00}";
        label-unfocused-underline = "#${colors.base08}";
        label-unfocused-padding = 1;

        label-urgent = "%index%";
        label-urgent-foreground = "#${colors.base0E}";
        label-urgent-background = "#${colors.base05}";
        label-urgent-underline = "#${colors.base08}";
        label-urgent-padding = 1;

        label-separator = "·";
        label-separator-padding = 0;
        label-separator-foreground = "#${colors.base05}";
        label-separator-background = "#${colors.base00}";
        scroll-down = "i3wm-wsprev";
      };
    };
    script = ''
      polybar top &
    '';
  };
}
