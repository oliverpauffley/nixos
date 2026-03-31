{
  flake.modules.homeManager.base = { inputs, config, pkgs, ... }:
    let
      nix-colors-lib = inputs.nix-colors.lib.contrib { inherit pkgs; };
      wallpaper = nix-colors-lib.nixWallpaperFromScheme {
        scheme = config.colorScheme;
        width = 2560;
        height = 1080;
        logoScale = 3.0;
      };
    in {
      home.file.".cache/noctalia/wallpapers.json" = {
        text = builtins.toJSON {
          defaultWallpaper = wallpaper;
          # wallpapers = { "DP-1" = "/path/to/monitor/wallpaper.png"; };
        };
      };

    };
}
