{
  flake.modules.nixos.base = { services.udisks2.enable = true; };
  flake.modules.homeManager.linux = {
    # auto mount removable disks
    services.udiskie = {
      enable = true;
      tray = "always";
    };
  };
}
