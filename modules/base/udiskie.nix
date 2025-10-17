{
  flake.modules.homeManager.base = {
    # auto mount removable disks
    services.udiskie = {
      enable = true;
      tray = "always";
    };
  };
}
