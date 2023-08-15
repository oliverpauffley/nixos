{ config, pkgs, lib, ... }:

let
  colors =
    config.colorscheme.colors;
in
{
  # Dependencies
  services.polybar = {
    enable = true;
    script = "polybar &";
    package = pkgs.polybar.override {
      alsaSupport = true;
    };
    config = {
      "bar/top" = {
        tray-position = "right";
        font-0 = "mononoki Nerd Font;40";
        font-1 = "GohuFont 14 Nerd Font;40";
        background = "#${colors.base00}";
        foreground = "#${colors.base05}";
        width = "100%";
        height = "4%";
        padding-right = 1;
        module-margin = 1;
        separator = "|";
        separator-foreground = "#${colors.base0B}";
        cursor-click = "pointer";
        cursor-scroll = "ns-resize";
        modules-left = [
          "xworkspaces"
          "xwindow"
          "now-playing"
        ];
        modules-right = [
          "filesystem"
          "pulseaudio"
          "memory"
          "cpu"
          "cputemp"
          "battery"
          "date"
        ];
      };
      "module/xworkspaces" = {
        type = "internal/xworkspaces";

        label-active-background = "#${colors.base00}";
        label-active-underline = "#${colors.base06}";
        label-active-padding-right = 1;

        label-occupied-padding-right = 1;

        label-urgent-background = "#${colors.base09}";
        label-urgent-padding-right = 1;

        label-empty-foreground = "#${colors.base04}";
        label-empty-padding-right = 1;
      };
      "module/xwindow" = {
        type = "internal/xwindow";
        label-maxlen = 32;
      };
      "module/pulseaudio" = {
        type = "internal/pulseaudio";

        format-volume-prefix = "VOL ";
        format-volume-prefix-foreground = "#${colors.base08}";

        label-muted = "muted";
        label-muted-foreground = "#${colors.base02}";
      };
      "module/memory" = {
        type = "internal/memory";

        interval = 2;

        format-prefix = "RAM ";
        format-prefix-foreground = "#${colors.base0A}";
      };
      "module/cpu" = {
        type = "internal/cpu";

        interval = 2;

        format-prefix = "CPU ";
        format-prefix-foreground = "#${colors.base08}";
      };
      "module/date" = {
        type = "internal/date";

        internal = 1;

        date = "%H:%M";
        date-alt = "%Y-%m-%d %H:%M:%S";

        label-foreground = "#${colors.base07}";
      };
    };
  };
}
