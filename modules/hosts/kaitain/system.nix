{
  flake.hosts."kaitain" = {
    description = "macbook";
    ipv4 = "192.168.1.60"; # TODO update to the machine's real address
  };
  flake.modules.darwin."hosts/kaitain" = {
    nixpkgs.hostPlatform = "aarch64-darwin";

    networking.hostName = "kaitain";
    networking.computerName = "kaitain";
    networking.localHostName = "kaitain";

    system.primaryUser = "ollie";
    system.stateVersion = 6;
  };
}
