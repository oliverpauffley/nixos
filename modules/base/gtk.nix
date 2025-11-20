{
  flake.modules.nixos.base = { pkgs, ... }: {
    environment.systemPackages = with pkgs; [ gtk3 ];
    programs.dconf.profiles.user.databases = [{
      settings."org/gnome/desktop/interface".color-scheme = "prefer-dark";
    }];
  };

  flake.modules.homeManager.base = { pkgs, config, inputs, ... }: {
    gtk = let nix-colors-lib = inputs.nix-colors.lib.contrib { inherit pkgs; };
    in {
      enable = true;
      font.name = "${config.fontProfiles.regular.family} 12";
      # Takes a scheme, ouputs a generated materia GTK theme
      theme.name = config.colorScheme.slug;
      theme.package =
        nix-colors-lib.gtkThemeFromScheme { scheme = config.colorScheme; };
      gtk3.extraConfig.gtk-application-prefer-dark-theme = 1;
      gtk4.extraConfig.gtk-application-prefer-dark-theme = 1;
    };
  };
}
