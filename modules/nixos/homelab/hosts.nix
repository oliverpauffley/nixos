{ lib, config, ... }:
# let
#   hostOptions = with lib;
#     with types; {
#       # icon = mkOption {
#       #   type = str;
#       #   default = null;
#       #   description = ''
#       #     host icon
#       #   '';
#       # };

#       description = mkOption {
#         type = str;
#         default = null;
#         description = ''
#           host description
#         '';
#       };

#       ipv4 = mkOption {
#         type = str;
#         description = ''
#           own ipv4 address
#         '';
#       };

#       dnsalias = mkOption {
#         type = nullOr (listOf str);
#         default = null;
#         description = ''
#           dnsalias for this host
#         '';
#       };

#       isDNS = mkOption {
#         type = bool;
#         default = false;
#         description = "Is this machine running core DNS";
#       };
#     };
{
  #   options = with lib; {
  #     modules.homelab.hosts = mkOption {
  #       type = with types; attrsOf (submodule [{ options = hostOptions; }]);
  #       description = "A host in my homelab";
  #     };
  #     modules.homelab.currentHost = mkOption {
  #       type = with types; submodule [{ options = hostOptions; }];
  #       default = config.modules.homelab.hosts.${config.networking.hostName};
  #       description = "The host that is described by this configuration";
  #     };
  #   };

  #   config = {
  #     warnings = lib.optional
  #       (!(config.modules.homelab.hosts ? ${config.networking.hostName}))
  #       "no network configuration for ${config.networking.hostName} found in ${
  #         ./hosts.nix
  #       }";
  #   };

  #   config.modules.homelab.hosts.caladan = {
  #     dnsalias = [ "caladan" "sonarr" "radarr" "dashboard" "plex" ];
  #     description = "homelab";
  #     ipv4 = "192.168.1.100";
  #   };

  #   config.modules.homelab.hosts.arrakis = {
  #     dnsalias = [ "arrakis" ];
  #     description = "work laptop";
  #     ipv4 = "192.168.1.159";
  #     isDNS = true;
  #   };
}
