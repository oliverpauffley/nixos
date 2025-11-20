{
  flake.modules.nixos.base = { services.udisks2.enable = true; };
  flake.modules.homeManager.base = {
    # auto mount removable disks
    services.udiskie = {
      enable = true;
      tray = "always";
    };
  };
}
