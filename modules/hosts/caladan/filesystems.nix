{
  flake.modules.nixos."hosts/caladan" = {
    fileSystems."/" = {
      device = "/dev/disk/by-uuid/76210159-710a-4441-a26b-e744aff2ac6e";
      fsType = "ext4";
    };

    fileSystems."/boot" = {
      device = "/dev/disk/by-uuid/725E-5CC0";
      fsType = "vfat";
    };

    fileSystems."/mnt/media" = {
      device = "/dev/disk/by-uuid/bb45b80e-016c-40ac-af2d-293d78a8bf0b";
      fsType = "ext4";
    };

    swapDevices = [ ];
  };
}
