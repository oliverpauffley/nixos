{
  flake.modules.homeManager.noctalia =
    {
      inputs,
      config,
      pkgs,
      ...
    }:
    let
      inherit (config.colorScheme) palette;
      vivendi = {
        dark = {
          mPrimary = "#${palette.base04}";
          mOnPrimary = "#${palette.base05}";
          mSecondary = "#${palette.base03}";
          mOnSecondary = "#${palette.base04}";
          mTertiary = "#${palette.base0D}";
          mOnTertiary = "#${palette.base00}";
          mSurface = "#${palette.base00}";
          mOnSurface = "#${palette.base05}";
          mSurfaceVariant = "#${palette.base01}";
          mOnSurfaceVariant = "#${palette.base05}";
          mHover = "#${palette.base02}";
          mOnHover = "#${palette.base05}";
          mError = "#${palette.base04}";
          mOnError = "#${palette.base00}";
          mOutline = "#${palette.base06}";
          mShadow = "#${palette.base00}";
          terminal = {
            background = "#${palette.base00}";
            foreground = "#${palette.base05}";
            cursor = "#${palette.base04}";
            cursorText = "#${palette.base04}";
            selectionBg = "#${palette.base0F}";
            selectionFg = "#${palette.base0E}";
            normal = {
              black = "#${palette.base00}";
              red = "#${palette.base04}";
              green = "#${palette.base0C}";
              yellow = "#${palette.base0A}";
              blue = "#${palette.base0B}";
              magenta = "#${palette.base0D}";
              cyan = "#${palette.base08}";
              white = "#${palette.base05}";
            };
            bright = {
              black = "#${palette.base00}";
              red = "#${palette.base04}";
              green = "#${palette.base0C}";
              yellow = "#${palette.base0A}";
              blue = "#${palette.base0B}";
              magenta = "#${palette.base0D}";
              cyan = "#${palette.base08}";
              white = "#${palette.base05}";
            };
          };
        };
      };
    in
    {
      imports = [ inputs.noctalia.homeModules.default ];

      programs.noctalia = {
        enable = true;
        customPalettes.vivendi = vivendi;
        settings = {
          bar.default = {
            center = [
              "group:g1"
              "cat"
            ];
            end = [
              "notifications"
              "volume"
              "brightness"
              "battery"
              "control-center"
              "session"
            ];
            margin_edge = 0;
            margin_ends = 0;
            padding = 12;
            radius = 0;
            scale = 1.0;
            start = [
              "workspaces"
              "launcher"
              "wallpaper"
              "network"
            ];
            thickness = 23;
            widget_spacing = 9;

            capsule_group = [
              {
                fill = "surface_variant";
                id = "g1";
                members = [
                  "clock"
                ];
                opacity = 1.0;
                padding = 6.0;
              }
            ];
            dock.monitors = [ "eDP-1" ];
            corner_radius_scale = 0.0;
            font_family = config.fontProfiles.monospace.family;
            polkit_agent = true;
            settings_show_advanced = true;
            ui_scale = 1.35;

            animation.speed = 1.5;

            panel = {
              clipboard_placement = "attached";
              launcher_placement = "attached";
              launcher_session_search = true;
            };

            shadow.alpha = 0.5;

          };
          theme = {
            source = "custom";
            custom_palette = "vivendi.json";
            templates = {
              enable_builtin_templates = false;
              enable_community_templates = false;
            };
          };
        };
      };
    };
}
