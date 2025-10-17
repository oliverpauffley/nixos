{
  flake.modules.nixos.base = { pkgs, ... }: {
    environment.systemPackages = with pkgs; [ gtk3 ];
  };

  flake.modules.homeManager.base = { pkgs, config, ... }: {
    gtk = {
      enable = true;
      font.name = "${config.fontProfiles.regular.family} 12";
      theme = {
        name = "Dracula";
        package = pkgs.dracula-theme;
      };
    };
  };
}
