{
  flake.modules.nixos.game = {
    boot.kernel.sysctl = { "vm.max_map_count" = 1048576; };
    programs.steam = {
      enable = true;
      remotePlay.openFirewall =
        true; # Open ports in the firewall for Steam Remote Play
      dedicatedServer.openFirewall =
        true; # Open ports in the firewall for Source Dedicated Server
    };
    programs.gamemode.enable = true;
  };
}
