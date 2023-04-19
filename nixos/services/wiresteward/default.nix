{ config, pkgs, lib, ... }:


let
  cfg = config.services.wiresteward;
in
with lib;

{
  options = {
    services.wiresteward = {
      enable = mkEnableOption (lib.mdDoc "Wiresteward wireguard tunnel");
    };
  };

  config = mkIf cfg.enable {
    systemd.services.wiresteward = {
      wantedBy = [ "multi-user.target" ];
      after = [ "network-online.target" ];
      requires = [ "network-online.target" ];
      description = "wiresteward vpn tunnel.";
      serviceConfig = {
        ExecStart = ''${pkgs.wiresteward}/bin/wiresteward --agent'';
      };
    };

    environment.systemPackages = [ pkgs.wiresteward ];

    # write default config.json
    environment.etc.wiresteward.source = ./config.json;
    environment.etc.wiresteward.target = ''wiresteward/config.json'';
  };

  # TODO firewall MTU settings
}
