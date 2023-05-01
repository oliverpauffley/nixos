{ config, lib, pkgs, ... }:

let mod = "Mod4";
in
{
  home.packages = with pkgs; [ dmenu maim i3lock ];
  xsession.windowManager.i3 =
    {
      enable = true;
      config = {
        workspaceAutoBackAndForth = true;
        modifier = mod;
        window = { titlebar = false; };

        keybindings = lib.mkOptionDefault {
          "${mod}+d" =
            "exec  rofi -show drun -modi drun -show-icons -matching fuzzy";
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
        startup = [{
          command = "systemctl --user restart polybar";
          always = true;
          notification = false;
        }];
      };
    };
}
