{
  flake.modules.nixos.base = {
    services.openssh = {
      enable = true;
      openFirewall = true;
      settings = {
        X11Forwarding = true;
        StreamLocalBindUnlink = "yes";
        PermitRootLogin = "no";
      };
    };
  };
}
