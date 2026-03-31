{
  flake.modules.homeManager.noctalia = { inputs, config, pkgs, ... }:
    let
      inherit (config.colorScheme) palette;
      colors = {
        mPrimary = palette.base04;
        mOnPrimary = palette.base00;
        mSecondary = palette.base0F;
        mOnSecondary = palette.base00;
        mTertiary = palette.base0D;
        mOnTertiary = palette.base00;
        mSurface = palette.base00;
        mOnSurface = palette.base05;
        mSurfaceVariant = palette.base01;
        mOnSurfaceVariant = palette.base05;
        mHover = palette.base02;
        mOnHover = palette.base05;
        mError = palette.base04;
        mOnError = palette.base00;
        mOutline = palette.base06;
        mShadow = palette.base00;
      };
    in {
      imports = [ inputs.noctalia.homeModules.default ];

      programs.noctalia-shell = {
        enable = true;

        settings = {
          bar = {
            position = "top";
            floating = false;
            backgroundOpacity = 0.9;
            widgets = {
              left = [
                { id = "Launcher"; }
                { id = "Clock"; }
                { id = "ActiveWindow"; }
                { id = "MediaMini"; }
              ];
              center = [{ id = "Workspace"; }];
              right =
                [ { id = "Tray"; } { id = "Battery"; } { id = "Volume"; } ];
            };
          };
          general = {
            animationSpeed = 1.0;
            radiusRatio = 0.2;
          };
          colorSchemes = {
            darkMode = true;
            useWallpaperColors = false;
          };
          ui = {
            fontDefault = config.fontProfiles.monospace.family;
            fontFixed = config.fontProfiles.regular.family;
          };
          location = { name = "Bournemouth"; };
        };

        colors = {
          mPrimary = "#${colors.mPrimary}";
          mOnPrimary = "#${colors.mOnPrimary}";
          mSecondary = "#${colors.mSecondary}";
          mOnSecondary = "#${colors.mOnSecondary}";
          mTertiary = "#${colors.mTertiary}";
          mOnTertiary = "#${colors.mOnTertiary}";
          mSurface = "#${colors.mSurface}";
          mOnSurface = "#${colors.mOnSurface}";
          mSurfaceVariant = "#${colors.mSurfaceVariant}";
          mOnSurfaceVariant = "#${colors.mOnSurfaceVariant}";
          mHover = "#${colors.mHover}";
          mOnHover = "#${colors.mOnHover}";
          mError = "#${colors.mError}";
          mOnError = "#${colors.mOnError}";
          mOutline = "#${colors.mOutline}";
          mShadow = "#${colors.mShadow}";
        };
      };
    };
}
