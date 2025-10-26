{
  flake.modules.nixos.base = { pkgs, ... }: {
    environment.systemPackages = with pkgs; [ gtk3 ];
  };

  flake.modules.homeManager.base = { pkgs, config, inputs, ... }: {
    gtk = let nix-colors-lib = inputs.nix-colors.lib.contrib { inherit pkgs; };
    in {
      enable = true;
      font.name = "${config.fontProfiles.regular.family} 12";
      # Takes a scheme, ouputs a generated materia GTK theme
      # Example:
      theme.name = "nix-colors";
      theme.package =
        nix-colors-lib.gtkThemeFromScheme { scheme = config.colorScheme; };
    };
  };
}
