{ pkgs, lib, config, ... }:
let
  theme = config;
in 
  {
    xsession.windowManager.i3 = {
      enable = true;
      package = pkgs.i3-gaps;

      config = rec {
        modifier = "Mod4";
        bars = [];
        colors = {
          background = "${theme.colours.bg}";
          focused = { background = "${theme.colours.bg}"; border = "${theme.colours.bg_critical}"; childBorder = "${theme.colours.text_unfocused}"; indicator = "${theme.colours.text_primary}"; text = "${theme.colours.text_primary}";};
          focusedInactive = { background = "${theme.colours.bg}"; border = "${theme.colours.bg_critical}"; childBorder = "${theme.colours.text_primary}"; indicator = "${theme.colours.text_primary}"; text = "${theme.colours.text_primary}";};
          unfocused = { background = "${theme.colours.bg_unfocused}"; border = "${theme.colours.bg}"; childBorder = "${theme.colours.bg}"; indicator = "${theme.colours.text_primary}"; text = "${theme.colours.text_unfocused}";};
        };


        window.border = 2;

        gaps = {
          inner = 5;
        };

        keybindings = lib.mkOptionDefault {
          "XF86AudioMute" = "exec amixer set Master toggle";
          "XF86AudioLowerVolume" = "exec amixer set Master 4%-";
          "XF86AudioRaiseVolume" = "exec amixer set Master 4%+";
          "XF86MonBrightnessDown" = "exec brightnessctl set 4%-";
          "XF86MonBrightnessUp" = "exec brightnessctl set 4%+";
          "${modifier}+Return" = "exec ${pkgs.alacritty}/bin/alacritty";
          "${modifier}+d" = "exec ${pkgs.rofi}/bin/rofi -modi drun -show drun";
          "${modifier}+Shift+d" = "exec ${pkgs.rofi}/bin/rofi -show window";
          "${modifier}+Ctrl+d" = ''exec ${pkgs.rofi}/bin/rofi -modi "clipboard:greenclip print" -show clipboard -run command "{cmd}"'';
          "${modifier}+Shift+x" = "exec systemctl suspend";
        };

        startup = [
          {
            command = "systemctl --user restart polybar.service";
            always = true;
            notification = false;
          }

          {
            command = "${pkgs.feh}/bin/feh --bg-scale ~/.config/background.png"; # TODO: fetch background from URL
            always = true;
            notification = false;
          }
          {
            command = "systemctl --user enable greenclip.service";
            always = true;
            notification = false;
          }
        ];
      };
    };
  }
