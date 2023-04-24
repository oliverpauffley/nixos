{ config, lib, pkgs, ... }:

let
  mod = "Mod4";
in
{
  home.packages = with pkgs; [ dmenu maim i3lock ];

  xsession.windowManager.i3 = {
    enable = true;
    config = {
      modifier = mod;
      colors =
        {
          focused = "#${config.colorScheme.colors.base05}";
          focusedInactive = "#${config.colorScheme.colors.base06}";
          unfocused = "#${config.colorScheme.colors.base00}";
        };
      window = {
        titlebar = false;
      };
      fonts = {
        names = [ "GohuFont Nerd Font" ];
        style = "Bold";
        size = 11.0;
      };

      keybindings = lib.mkOptionDefault {
        "${mod}+d" = "exec ${pkgs.dmenu}/bin/dmenu_run";
        "${mod}+Return" = "exec ${pkgs.wezterm}/bin/wezterm";
        "${mod}+x" =
          "exec sh -c '${pkgs.maim}/bin/maim -s | xclip -selection clipboard -t image/png'";
        "${mod}+Shift+x" =
          "exec sh -c '${pkgs.i3lock}/bin/i3lock -c 222222 & sleep 5 && xset dpms force of'";

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

        # Fullscreen
        "${mod}+f" = "fullscreen toggle";

        # My multi monitor setup
        "${mod}+m" = "move workspace to output DP-2";
        "${mod}+Shift+m" = "move workspace to output DP-5";
      };

      bars = [ ];
    };
  };
}
