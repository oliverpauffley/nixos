{ lib, config, ... }:
let
  domain = "home.lab";
  # find the config that is running coreDNS
  dnsServerIP = (lib.findFirst (x: x.isDNS) null
    (builtins.attrValues config.flake.hosts)).ipv4;
  servers = [ dnsServerIP "1.1.1.1" "9.9.9.9" "8.8.8.8" ];

  # get ips with their aliases
  aliasIps = lib.flatten (lib.mapAttrsToList (name: host:
    let alias = lib.optionals (host.dnsalias != null) host.dnsalias;
    in map (entry: {
      name = entry;
      ip = host.ipv4;
    }) alias) config.flake.hosts);

in {
  flake.modules.nixos.base = { lib, ... }: {
    programs.nm-applet.enable = true;
    networking = {
      useDHCP = false;
      networkmanager = {
        enable = true;
        dns = lib.mkDefault "none";
        insertNameservers = servers;
      };

      inherit domain;
      extraHosts = ''

        # ADM
        192.168.1.100 traefik.home.lab

        # Hosts
        ${lib.concatStringsSep "\n" (lib.mapAttrsToList (hostname: hostinfo:
          "${hostinfo.ipv4} ${hostname}.${domain} ${hostname}")
          config.flake.hosts)}

        # Alias
        ${lib.concatMapStringsSep "\n"
        (host: "${host.ip} ${host.name}.${domain} ${host.name}") aliasIps}
      '';
    };
    systemd = {
      services.NetworkManager-wait-online.enable = false;
      network.wait-online.enable = false;
    };
  };
}
