{
  config,
  lib,
  pkgs,
  ...
}: {
  programs.waybar = {
    enable = true;
    style = let
      inherit (config.colorscheme) colors;
      # css
    in ''
      * {
        font-family: ${config.fontProfiles.regular.family}, ${config.fontProfiles.monospace.family};
        font-size: 8pt;
        padding: 0 4px;
      }

      .modules-right {
        margin-right: -8px;
      }

      .modules-left {
        margin-left: -8px;
      }

      window#waybar.top {
        opacity: 0.95;
        padding: 0;
        background-color: #${colors.base00};
        border: 2px solid #${colors.base0C};
        border-radius: 5px;
      }
      window#waybar.bottom {
        opacity: 0.90;
        background-color: #${colors.base00};
        border: 2px solid #${colors.base0C};
        border-radius: 5px;
      }

      window#waybar {
        color: #${colors.base05};
      }

      #workspaces button {
        background-color: #${colors.base01};
        color: #${colors.base05};
        margin: 4px;
      }
      #workspaces button.hidden {
        background-color: #${colors.base00};
        color: #${colors.base04};
      }
      #workspaces button.focused,
      #workspaces button.active {
        background-color: #${colors.base0A};
        color: #${colors.base00};
      }

      #clock {
        background-color: #${colors.base0C};
        color: #${colors.base00};
        padding-left: 8px;
        padding-right: 8px;
        margin-top: 0;
        margin-bottom: 0;
        border-radius: 5px;
      }

      #custom-menu {
        background-color: #${colors.base0C};
        color: #${colors.base00};
        padding-left: 15px;
        padding-right: 22px;
        margin-left: 0;
        margin-right: 10px;
        margin-top: 0;
        margin-bottom: 0;
        border-radius: 10px;
      }
      #custom-hostname {
        background-color: #${colors.base0C};
        color: #${colors.base00};
        padding-left: 15px;
        padding-right: 18px;
        margin-right: 0;
        margin-top: 0;
        margin-bottom: 0;
        border-radius: 10px;
      }
      #tray {
        color: #${colors.base05};
      }
    '';
    settings = [
      {
        height = 34;
        layer = "top";
        position = "top";
        tray = {spacing = 10;};
        "wlr/workspaces" = {on-click = "activate";};
        modules-left = ["wlr/workspaces"];
        modules-right = ["pulseaudio" "network" "battery" "clock"];
        battery = {
          format = "{capacity}% {icon}   ";
          format-alt = "{time} {icon}";
          format-charging = "{capacity}%     ";
          format-icons = ["" "" "" "" ""];
          format-plugged = "{capacity}% ";
          states = {
            critical = 15;
            warning = 30;
          };
        };
        clock = {
          format-alt = "{:%Y-%m-%d}";
          tooltip-format = "{:%Y-%m-%d | %H:%M}";
        };
        network = {
          interval = 1;
          format-alt = "{ifname}: {ipaddr}/{cidr}";
          format-disconnected = "Disconnected ⚠";
          format-ethernet = "{ifname}";
          format-linked = "{ifname} ";
          format-wifi = "{essid} ";
        };
        pulseaudio = {
          format = "{volume}%  {icon}  {format_source}";
          format-bluetooth = "{volume}%  {icon}  {format_source}";
          format-bluetooth-muted = "  {icon}  {format_source}";
          format-icons = {
            car = "";
            default = ["" "" ""];
            handsfree = "";
            headphones = "";
            headset = "";
            phone = "";
            portable = "";
          };
          format-muted = " {format_source}";
          format-source = "{volume}% ";
          format-source-muted = "";
          on-click = "pavucontrol";
        };
      }
    ];
  };
}
