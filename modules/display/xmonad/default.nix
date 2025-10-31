{
  flake.modules.nixos.xmonad = {
    services.xserver.windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };
  };

  flake.modules.homeManager.xmonad = { pkgs, config, ... }:
    let inherit (config.colorscheme) palette;
    in {
      home.packages = with pkgs; [ scrot xdotool yad ];
      programs.xmobar = {
        enable = true;
        extraConfig = ''
          Config {

             -- appearance
               font =         "${config.fontProfiles.monospace.family} 12"
             , bgColor =      "#${palette.base01}"
             , fgColor =      "#${palette.base03}"
             , position =     TopP 0 25
             , border =       BottomB
             , borderColor =  "#${palette.base0B}"

             -- layout
             , sepChar =  "%"   -- delineator between plugin names and straight text
             , alignSep = "}{"  -- separator between left-right alignment
             , template = "%XMonadLog% }{ %date% | %battery% "

             -- general behavior
             , lowerOnStart =     True    -- send to bottom of window stack on start
             , hideOnStart =      False   -- start with window unmapped (hidden)
             , allDesktops =      True    -- show on all desktops
             , overrideRedirect = True    -- set the Override Redirect flag (Xlib)
             , pickBroadest =     False   -- choose widest display (multi-monitor)
             , persistent =       True    -- enable/disable hiding (True = disabled)

             -- plugins
             --   Numbers can be automatically colored according to their value. xmobar
             --   decides color based on a three-tier/two-cutoff system, controlled by
             --   command options:
             --     --Low sets the low cutoff
             --     --High sets the high cutoff
             --
             --     --low sets the color below --Low cutoff
             --     --normal sets the color between --Low and --High cutoffs
             --     --High sets the color above --High cutoff
             --
             --   The --template option controls how the plugin is displayed. Text
             --   color can be set by enclosing in <fc></fc> tags. For more details
             --   see http://projects.haskell.org/xmobar/#system-monitor-plugins.
             , commands =

                  -- weather monitor
                  [ Run Weather "RJTT" [ "--template", "<skyCondition> | <fc=#4682B4><tempC></fc>°C | <fc=#4682B4><rh></fc>% | <fc=#4682B4><pressure></fc>hPa"
                                       ] 36000

                  -- network activity monitor (dynamic interface resolution)
                  , Run DynNetwork     [ "--template" , "<dev>: <tx>kB/s|<rx>kB/s"
                                       , "--Low"      , "1000"       -- units: B/s
                                       , "--High"     , "5000"       -- units: B/s
                                       , "--low"      , "darkgreen"
                                       , "--normal"   , "darkorange"
                                       , "--high"     , "darkred"
                                       ] 10

                  -- battery monitor
                  , Run Battery        [ "--template" , "Batt: <acstatus>"
                                       , "--Low"      , "10"        -- units: %
                                       , "--High"     , "80"        -- units: %
                                       , "--low"      , "darkred"
                                       , "--normal"   , "darkorange"
                                       , "--high"     , "darkgreen"

                                       , "--" -- battery specific options
                                                 -- discharging status
                                                 , "-o"	, "<left>% (<timeleft>)"
                                                 -- AC "on" status
                                                 , "-O"	, "<fc=#dAA520>Charging</fc>"
                                                 -- charged status
                                                 , "-i"	, "<fc=#006000>Charged</fc>"
                                       ] 50
                  , Run XMonadLog
                  -- time and date indicator
                  --   (%F = y-m-d date, %a = day of week, %T = h:m:s time)
                  , Run Date           "<fc=#ABABAB>%F (%a) %T</fc>" "date" 10

                  ]
             }
        '';
      };

      services.stalonetray = {
        enable = true;
        extraConfig = ''
          decorations none
          transparent false
          dockapp_mode none
          geometry 1x1-0+0
          background "#${palette.base01}"
          kludges force_icons_size
          grow_gravity NW
          icon_gravity NW
          icon_size 18
          sticky true
          #window_strut none
          window_type dock
          window_layer bottom
          no_shrink false
          skip_taskbar true
        '';
      };

      xsession = {
        enable = true;
        windowManager.xmonad = let
          xmonadConfig = pkgs.writeTextFile {
            name = "xmonad-config";
            text = builtins.readFile ./config.hs;
          };
        in {
          enable = true;
          enableContribAndExtras = true;
          extraPackages = hp: [ hp.dbus hp.monad-logger hp.xmonad-contrib ];
          config = xmonadConfig;
          libFiles = {
            "Colors.hs" = pkgs.writeText "Colors.hs" ''
              module Colors where

              import XMonad
              colorBg = "#${palette.base00}"
              colorFg = "#${palette.base05}"
              color01 = "#${palette.base01}"
              color02 = "#${palette.base08}"
              color03 = "#${palette.base0B}"
              color04 = "#${palette.base0A}"
              color05 = "#${palette.base0E}"
              color06 = "#${palette.base0F}"
              color07 = "#${palette.base0D}"
              color08 = "#${palette.base07}"

              -- Select focus and secondary color
              colorFocus = color02
              colorSecondary = color07
            '';
          };
        };
      };
    };
}
