{
  flake.modules.homeManager.base = {
    services.random-background = {
      enable = true;
      display = "fill";
      imageDirectory = "%h/wallpapers";
      interval = "1h";
    };
  };
}
