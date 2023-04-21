{ config, lib, pkgs, ... }:

let
  mod = "Mod4";
in
{
  programs.i3status-rust = {
    enable = true;
    bars = {
      top = {
        blocks = [
          {
            block = "time";
            interval = 60;
            format = "%a %d/%m %k:%M %p";
          }
        ];
      };
    };
  };

  home.packages = with pkgs; [ dmenu maim i3lock ];
  xsession.windowManager.i3 = {
    enable = true;
    config = {
      modifier = mod;

      fonts = [ "DejaVu Sans Mono, FontAwesome 6" ];

      keybindings = lib.mkOptionDefault {
        "${mod}+d" = "exec ${pkgs.dmenu}/bin/dmenu_run";
        "${mod}+return" = "exec ${pkgs.wezterm}/bin/wezterm";
        "${mod}+x" = "exec sh -c '${pkgs.maim}/bin/maim -s | xclip -selection clipboard -t image/png'";
        "${mod}+Shift+x" = "exec sh -c '${pkgs.i3lock}/bin/i3lock -c 222222 & sleep 5 && xset dpms force of'";

        # Focus
        "${mod}+j" = "focus left";
        "${mod}+k" = "focus down";
        "${mod}+l" = "focus up";
        "${mod}+semicolon" = "focus right";

        # Move
        "${mod}+Shift+j" = "move left";
        "${mod}+Shift+k" = "move down";
        "${mod}+Shift+l" = "move up";
        "${mod}+Shift+semicolon" = "move right";

        # My multi monitor setup
        "${mod}+m" = "move workspace to output DP-2";
        "${mod}+Shift+m" = "move workspace to output DP-5";
      };

      bars = [
        {
          position = "top";
          statusCommand = "${pkgs.i3status-rust}/bin/i3status-rs ~/.config/i3status-rust/config-top.toml";
        }
      ];
    };
  };
}
