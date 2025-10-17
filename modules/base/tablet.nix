{
  flake.modules.nixos.base = {
    hardware.opentabletdriver = {
      enable = true;
      daemon.enable = true;
    };

  };

}
