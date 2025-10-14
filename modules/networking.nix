{
  # TODO fix dns
  #
  # let
  #   # find the config that is running coreDNS
  #   dnsServerIP = (lib.findFirst (x: x.isDNS) null
  #     (builtins.attrValues config.modules.homelab.hosts)).ipv4;
  #   servers = [ dnsServerIP "1.1.1.1" "9.9.9.9" "8.8.8.8" ];

  #   # get ips with their aliases
  #   aliasIps = lib.flatten (lib.mapAttrsToList (name: host:
  #     let alias = lib.optionals (host.dnsalias != null) host.dnsalias;
  #     in map (entry: {
  #       name = entry;
  #       ip = host.ipv4;
  #     }) alias) config.modules.homelab.hosts);

  #   domain = "home.lab";
  # extraHosts = ''

  #   # ADM
  #   192.168.1.100 traefik.home.lab

  #   # Hosts
  #   ${lib.concatStringsSep "\n" (lib.mapAttrsToList (hostname: hostinfo:
  #     "${hostinfo.ipv4} ${hostname}.${domain} ${hostname}")
  #     config.modules.homelab.hosts)}

  #   # Alias
  #   ${lib.concatMapStringsSep "\n"
  #   (host: "${host.ip} ${host.name}.${domain} ${host.name}") aliasIps}
  # '';
  # in {
  flake.modules.nixos.base = {
    programs.nm-applet.enable = true;
    services.resolved = { enable = true; };
    networking = {
      # networkmanager.insertNameservers = servers;
      useDHCP = false;
      networkmanager = {
        enable = true;
        insertNameservers = [ "1.1.1.1" ];
      };
    };
    systemd = {
      services.NetworkManager-wait-online.enable = false;
      network.wait-online.enable = false;
    };
  };
}
