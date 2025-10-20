{
  flake.modules.homeManager.base = {
    services.random-background = {
      enable = true;
      display = "scale";
      enableXinerama = false;
      imageDirectory = "%h/wallpapers";
      interval = "1h";
    };
  };
}
