{
  flake.modules.nixos.base = { pkgs, ... }: {
    config.fonts.packages = with pkgs; [
      nerd-fonts.mononoki
      nerd-fonts.gohufont
      nerd-fonts.droid-sans-mono
      nerd-fonts.departure-mono
      luculent
      jost
    ];
    config.fonts.fontDir.enable = true;
    config.fonts.fontDir.decompressFonts = true;
  };

  flake.modules.homeManager.base = { pkgs, ... }: {
    config = {
      fonts.fontconfig.enable = true;
      fontProfiles = {
        enable = true;
        monospace = {
          family = "DepartureMono Nerd Font";
          package = pkgs.nerd-fonts.departure-mono;
        };
        regular = {
          family = "Mononoki Nerd Font";
          package = pkgs.nerd-fonts.mononoki;
        };
      };
    };
  };
}
