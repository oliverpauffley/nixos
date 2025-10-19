{
  flake.modules.nixos."hosts/arrakis" = {
    fileSystems."/" = {
      device = "/dev/disk/by-uuid/43ff5d13-227d-473e-b765-31efc0022dcb";
      fsType = "ext4";
    };

    fileSystems."/boot" = {
      device = "/dev/disk/by-uuid/2B82-8E1B";
      fsType = "vfat";
    };

    swapDevices =
      [{ device = "/dev/disk/by-uuid/67347012-b498-4ac6-84d6-a8762b7fdc19"; }];
  };
}
