{
  flake.modules.nixos.work = { pkgs, lib, config, inputs, ... }:
    let cfg = config.services.wiresteward;
    in {
      options = {
        services.wiresteward = {
          enable =
            lib.mkEnableOption (lib.mdDoc "Wiresteward wireguard tunnel");
        };
      };

      config = lib.mkIf cfg.enable {
        nixpkgs.overlays = [ inputs.self.overlays.default ];
        systemd.services.wiresteward = {
          wantedBy = [ "multi-user.target" ];
          after = [ "network-online.target" ];
          requires = [ "network-online.target" ];
          description = "wiresteward vpn tunnel.";
          serviceConfig = {
            ExecStart = "${pkgs.local.wiresteward}/bin/wiresteward --agent";
          };
        };

        environment.systemPackages = [ pkgs.local.wiresteward ];

        # write default config.json
        environment.etc.wiresteward.source = ./config.json;
        environment.etc.wiresteward.target = "wiresteward/config.json";

        networking.firewall.extraCommands =
          "iptables -w 60 -t mangle -A POSTROUTING -p tcp --tcp-flags SYN,RST SYN -j TCPMSS --clamp-mss-to-pmtu";
      };
    };
}
