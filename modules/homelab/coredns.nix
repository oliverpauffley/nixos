{ config, ... }: {
  flake.modules.nixos.coredns = { pkgs, lib, ... }:
    let
      ttl = 180;
      domain = "home.lab";

      # find the config that is running coreDNS
      dnsServerIP = (lib.findFirst (x: x.isDNS) null
        (builtins.attrValues config.flake.hosts)).ipv4;
      # TODO define config option here to turn on coredns and give it the host IP
      # Get Hosts IP
      hostsIps = lib.mapAttrsToList (name: host: {
        inherit name;
        ip = host.ipv4;
      }) config.flake.hosts;

      # Function
      # Get Alias IP
      aliasIps = lib.flatten (lib.mapAttrsToList (name: host:
        let alias = lib.optionals (host.dnsalias != null) host.dnsalias;
        in map (entry: {
          name = entry;
          ip = host.ipv4;
        }) alias) config.flake.hosts);

      fileZone = pkgs.writeText "h.zone" ''
        $ORIGIN ${domain}.
        @       IN SOA ns nomail (
                1         ; Version number
                60        ; Zone refresh interval
                30        ; Zone update retry timeout
                180       ; Zone TTL
                3600)     ; Negative response TTL

        ns ${toString ttl} IN A ${dnsServerIP}

        ; hosts
        ${lib.concatMapStringsSep "\n"
        (host: "${host.name} ${toString ttl} IN A ${host.ip}") hostsIps}

        ; alias
        ${lib.concatMapStringsSep "\n"
        (host: "${host.name} ${toString ttl} IN A ${host.ip}") aliasIps}
      '';

    in {
      config = {
        services.coredns.enable = true;

        networking.firewall.allowedTCPPorts = [ 53 9153 ];
        networking.firewall.allowedUDPPorts = [ 53 ];

        services.coredns.config = ''
                . {
                    forward . 9.9.9.9 1.1.1.1 1.0.0.1 8.8.8.8 8.8.4.4
                    cache
                    log
                  }

                ${domain} {
                    file ${fileZone}
          #           log
                  }
        '';
      };
    };
}
