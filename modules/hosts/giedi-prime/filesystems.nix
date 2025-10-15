{
  flake.modules.nixos."hosts/giedi-prime" = {
    fileSystems."/" = {
      device = "/dev/disk/by-uuid/c9e4749b-1763-4225-b2e0-a263dcd306b2";
      fsType = "ext4";
    };

    fileSystems."/boot" = {
      device = "/dev/disk/by-uuid/6A79-ECFC";
      fsType = "vfat";
    };

    swapDevices = [ ];
  };
}
