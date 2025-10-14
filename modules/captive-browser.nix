{
  flake.modules.nixos.laptop = { config, ... }:
    {
      # browser specifically for getting through network logins
      # programs.captive-browser.enable = true;
      # TODO how to get interface name?
      #programs.captive-browser.interface = cfg.networkInterface;
    };
}
