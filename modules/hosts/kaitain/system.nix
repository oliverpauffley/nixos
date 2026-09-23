{
  flake.hosts."kaitain" = {
    description = "macbook";
    ipv4 = "192.168.0.200";
  };
  flake.modules.darwin."hosts/kaitain" = { config, targets, ... }: {
    nixpkgs.hostPlatform = "aarch64-darwin";
    # allow building both mac platforms
    nix.extraOptions = ''
      extra-platforms = x86_64-darwin aarch64-darwin
    '';

    networking.hostName = "kaitain";
    networking.computerName = "kaitain";
    networking.localHostName = "kaitain";

    environment.systemPath = [
      "/run/current-system/sw/bin"
      "/nix/var/nix/profiles/default/bin"
    ];

    system.primaryUser = "ollie";
    system.stateVersion = 6;
    security.pam.services.sudo_local.touchIdAuth = true;
    security.sudo.extraConfig = ''
      Defaults !lecture
      Defaults pwfeedback
      Defaults timestamp_timeout = 300
    '';

    system.defaults = {
      dock.autohide = true;
      NSGlobalDomain = {
        AppleICUForce24HourTime = true;
        AppleInterfaceStyle = "Dark";
        KeyRepeat = 1; # Fastest
        InitialKeyRepeat = 15;
      };
      CustomUserPreferences = {
        "org.hammerspoon.Hammerspoon" = {
          MJConfigFile = "~/.config/hammerspoon/init.lua";
        };
      };

    };
    home-manager.useUserPackages = true;

    homebrew = {
      enable = true;
      casks = [
        "hammerspoon"
      ];
    };
  };
}
