{ config, lib, pkgs, ... }:
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
        monitor = "eDP-1";
        monitor-fallback = "HDMI-0";
        width = "100%";
        height = "2%";
        radius = 0;

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
        label-focused-padding = 1;

        label-unfocused = "%index%";
        label-unfocused-padding = 1;

        label-urgent = "%index%";
        label-urgent-padding = 1;

        label-separator = "·";
        label-separator-padding = 0;
        scroll-down = "i3wm-wsprev";
      };
    };
    script = ''
      polybar &
    '';
  };
}
