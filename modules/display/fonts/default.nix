{
  flake.modules.nixos.base = { pkgs, ... }: {
    config.fonts.packages = with pkgs; [
      nerd-fonts.mononoki
      nerd-fonts.gohufont
      nerd-fonts.droid-sans-mono
      departure-mono
      luculent
    ];
    config.fonts.fontDir.enable = true;
    config.fonts.fontDir.decompressFonts = true;
  };

  flake.modules.homeManager.base = { pkgs, ... }: {
    config.fonts.fontconfig.enable = true;
    config = {
      fontProfiles = {
        enable = true;
        monospace = {
          family = "Departure Mono";
          package = pkgs.departure-mono;
        };
        regular = {
          family = "Mononoki Nerd Font";
          package = pkgs.nerd-fonts.mononoki;
        };
      };
    };
  };
}
