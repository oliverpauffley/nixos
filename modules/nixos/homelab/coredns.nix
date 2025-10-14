{ lib, config, pkgs, ... }:
# let
#   ttl = 180;
#   cfg = config.modules.homelab.coredns;
#   domain = "home.lab";

#   # Function
#   # Get Hosts IP
#   hostsIps = lib.mapAttrsToList (name: host: {
#     inherit name;
#     ip = host.ipv4;
#   }) config.modules.homelab.hosts;

#   # Function
#   # Get Alias IP
#   aliasIps = lib.flatten (lib.mapAttrsToList (name: host:
#     let alias = lib.optionals (host.dnsalias != null) host.dnsalias;
#     in map (entry: {
#       name = entry;
#       ip = host.ipv4;
#     }) alias) config.modules.homelab.hosts);

#   fileZone = pkgs.writeText "h.zone" ''
#     $ORIGIN ${domain}.
#     @       IN SOA ns nomail (
#             1         ; Version number
#             60        ; Zone refresh interval
#             30        ; Zone update retry timeout
#             180       ; Zone TTL
#             3600)     ; Negative response TTL

#     ns ${toString ttl} IN A ${config.modules.homelab.currentHost.ipv4}

#     ; hosts
#     ${lib.concatMapStringsSep "\n"
#     (host: "${host.name} ${toString ttl} IN A ${host.ip}") hostsIps}

#     ; alias
#     ${lib.concatMapStringsSep "\n"
#     (host: "${host.name} ${toString ttl} IN A ${host.ip}") aliasIps}
#   '';
{
  #   options.modules.homelab.coredns = {
  #     enable = lib.mkEnableOption "Enable coredns service";
  #   };

  #   config = lib.mkIf cfg.enable {
  #     services.coredns.enable = true;

  #     networking.firewall.allowedTCPPorts = [ 53 9153 ];
  #     networking.firewall.allowedUDPPorts = [ 53 ];

  #     services.coredns.config = ''
  #       . {
  #           forward . 9.9.9.9 1.1.1.1 1.0.0.1 8.8.8.8 8.8.4.4
  #           cache
  #           log
  #         }

  #       ${domain} {
  #           file ${fileZone}
  #           log
  #         }
  #     '';
  #   };
}
