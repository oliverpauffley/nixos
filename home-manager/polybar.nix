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
        width = "100%";
        height = "2%";
        radius = 0;
        # Just sticking them together in the center for now
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
        scroll-down = "i3wm-wsprev";
      };
    };
    script = ''
      polybar top &
    '';
  };
  systemd.user.services.polybar = {
    Install.WantedBy = [ "graphical-session.target" ];
  };
}
