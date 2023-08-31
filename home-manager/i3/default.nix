{ config, pkgs, lib, ... }:

let
  colors = config.colorscheme.colors;
  mod = "Mod4";
in {
  # Dependencies
  home.packages = with pkgs; [
    wmctrl # For rofi power menu
    i3lock
    maim
    xdotool
    unclutter # Active window screenshots
    xsel # Disable middle mouse paste
    rofi
  ];

  programs.i3status-rust = {
    enable = true;
    bars = {
      default = {
        blocks = [
          { block = "sound"; }
          {
            block = "battery";
            format = " $icon $percentage ";
          }
          {
            block = "time";
            interval = 60;
            format = " $timestamp.datetime(f:'%a %d/%m %R') ";
          }
        ];
        settings = {
          theme = {
            theme = "solarized-dark";
            overrides = {
              idle_bg = "#${colors.base00}";
              idle_fg = "#${colors.base05}";
              info_bg = "#${colors.base0C}";
              info_fg = "#${colors.base00}";
              good_bg = "#${colors.base0B}";
              good_fg = "#${colors.base00}";
              warning_bg = "#${colors.base0A}";
              warning_fg = "#${colors.base00}";
              critical_bg = "#${colors.base08}";
              critical_fg = "#${colors.base00}";
            };
          };
        };
        icons = "awesome5";
      };
    };
  };

  programs.rofi = {
    enable = true;
    font = config.fontProfiles.monospace.family + " 14";
    theme = let
      # Use `mkLiteral` for string-like values that should show without
      # quotes, e.g.:
      # {
      #   foo = "abc"; => foo: "abc";
      #   bar = mkLiteral "abc"; => bar: abc;
      # };
      inherit (config.lib.formats.rasi) mkLiteral;
    in {
      "*" = {
        color-enabled = true;
        color-window = mkLiteral "#${colors.base01}";
        color-separator = mkLiteral "#${colors.base05}";
        color-background = mkLiteral "#${colors.base01}";

        # ! Property Name     BG       FG       BG-alt   Head-BG  Head-FG
        # rofi.color-normal:  #3a3432, #a5a2a2, #3a3432, #3a3432, #f7f7f7
        # rofi.color-active:  #3a3432, #01a0e4, #3a3432, #3a3432, #01a0e4
        # rofi.color-urgent:  #3a3432, #db2d20, #3a3432, #3a3432, #db2d20

        # ! Set the desired separator style
        # rofi.separator-style: solid
      };

    };
    # theme = "
    # ! Base16 3024
    # ! Author: Jan T. Sott (http://github.com/idleberg)

    # ! base00: #090300
    # ! base01: #3a3432
    # ! base02: #4a4543
    # ! base03: #5c5855
    # ! base04: #807d7c
    # ! base05: #a5a2a2
    # ! base06: #d6d5d4
    # ! base07: #f7f7f7
    # ! base08: #db2d20
    # ! base09: #e8bbd0
    # ! base0A: #fded02
    # ! base0B: #01a252
    # ! base0C: #b5e4f4
    # ! base0D: #01a0e4
    # ! base0E: #a16a94
    # ! base0F: #cdab53

    # ! Enable the extended coloring options

    # ! Property Name     BG       Border   Separator
    # rofi.color-window:  #3a3432, #3a3432, #090300

    # ! Property Name     BG       FG       BG-alt   Head-BG  Head-FG
    # rofi.color-normal:  #3a3432, #a5a2a2, #3a3432, #3a3432, #f7f7f7
    # rofi.color-active:  #3a3432, #01a0e4, #3a3432, #3a3432, #01a0e4
    # rofi.color-urgent:  #3a3432, #db2d20, #3a3432, #3a3432, #db2d20

    # ! Set the desired separator style
    # rofi.separator-style: solid
    # ";
  };

  # Screenshots
  services.flameshot = {
    enable = true;
    settings = {
      General = {
        disabledTrayIcon = true;
        showStartupLaunchMessage = false;
        showHelp = false;
        showSidePanelButton = false;
        uiColor = "#${colors.base00}";
        contrastUiColor = "#${colors.base06}";
        drawColor = "#${colors.base05}";
      };
    };
  };

  xsession.windowManager.i3 = {
    enable = true;
    config = {
      workspaceAutoBackAndForth = true;
      window.titlebar = false;
      defaultWorkspace = "workspace number 1"; # defaults to 10 for some reason
      workspaceOutputAssign = [
        {
          workspace = "1";
          output = "primary";
        }
        {
          workspace = "2";
          output = "primary";
        }
        {
          workspace = "3";
          output = "primary";
        }
        {
          workspace = "4";
          output = "primary";
        }
        {
          workspace = "5";
          output = "primary";
        }
        {
          workspace = "6";
          output = "nonprimary primary";
        }
        {
          workspace = "7";
          output = "nonprimary primary";
        }
        {
          workspace = "8";
          output = "nonprimary primary";
        }
        {
          workspace = "9";
          output = "nonprimary primary";
        }
        {
          workspace = "10";
          output = "nonprimary primary";
        }
      ];
      modifier = mod;
      colors = {
        focused = {
          border = "#${colors.base00}";
          background = "#${colors.base00}";
          text = "#${colors.base05}";
          indicator = "#${colors.base08}";
          childBorder = "#${colors.base09}";
        };
        focusedInactive = {
          border = "#${colors.base00}";
          background = "#${colors.base00}";
          text = "#${colors.base05}";
          indicator = "#${colors.base08}";
          childBorder = "#${colors.base09}";
        };
        unfocused = {
          border = "#${colors.base01}";
          background = "#${colors.base01}";
          text = "#${colors.base05}";
          indicator = "#${colors.base02}";
          childBorder = "#${colors.base02}";
        };
      };
      startup = [{
        command = "nm-applet &";
        always = true;
        notification = false;
      }];
      keybindings = lib.mkOptionDefault {
        "${mod}+Return" = "exec alacritty";
        "${mod}+Shift+e" =
          "exec i3-msg exit"; # bypass default session exit confirmation menu
        "${mod}+space" = "exec i3-msg open"; # Open blank space

        # Audio controls
        "XF86AudioRaiseVolume" =
          "exec --no-startup-id pactl set-sink-volume @DEFAULT_SINK@ +10%";
        "XF86AudioLowerVolume" =
          "exec --no-startup-id pactl set-sink-volume @DEFAULT_SINK@ -10%";
        "XF86AudioMute" =
          "exec --no-startup-id pactl set-sink-mute @DEFAULT_SINK@ toggle";
        "XF86AudioMicMute" =
          "exec --no-startup-id pactl set-source-mute @DEFAULT_SOURCE@ toggle";

        # Media player controls
        "XF86AudioPlay" = "exec playerctl play-pause";
        "XF86AudioNext" = "exec playerctl next";
        "XF86AudioPrev" = "exec playerctl previous";

        # Window resizing
        "${mod}+Ctrl+Left" = "resize shrink width 16px or 1ppt";
        "${mod}+Ctrl+Right" = "resize grow width 16px or 1ppt";
        "${mod}+Ctrl+Up" = "resize grow height 16px or 1ppt";
        "${mod}+Ctrl+Down" = "resize shrink height 16px or 1ppt";

        # Screenshots
        "Print" = "exec flameshot gui";
        # For some reason -u/--hidecursor tag makes image blank
        # So, we hide then unhide the cursor
        "${mod}+Print" = ''
          exec (unclutter -idle 0 &) && maim -i "$(xdotool getactivewindow)" | xclip -selection clipboard -t image/png && killall unclutter'';

        # Remove dmenu
        "${mod}+d" = "exec rofi -show drun -theme";
      };
      bars = [{
        position = "top";
        statusCommand =
          "${pkgs.i3status-rust}/bin/i3status-rs config-default.toml";
        command = "${pkgs.i3}/bin/i3bar -t";
        fonts = {
          names = [ config.fontProfiles.monospace.family ];
          size = 8.0;
        };

        trayOutput = "primary";
        colors = {
          background = "#${colors.base00}";
          statusline = "#${colors.base01}";
          separator = "#${colors.base0B}";
          focusedWorkspace = {
            border = "#${colors.base0A}";
            background = "#${colors.base02}";
            text = "#${colors.base05}";
          };
        };
      }];
    };
  };
}
