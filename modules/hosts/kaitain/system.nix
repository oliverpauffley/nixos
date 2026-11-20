{
  flake.hosts."kaitain" = {
    description = "macbook";
    ipv4 = "192.168.0.200";
  };
  flake.modules.darwin."hosts/kaitain" =
    {
      config,
      targets,
      pkgs,
      ...
    }:
    {
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

      environment.variables = {
        "NODE_EXTRA_CA_CERTS" = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
      };

      system.primaryUser = "ollie";
      system.stateVersion = 6;
      security.pki.certificateFiles = [ "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt" ];
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
