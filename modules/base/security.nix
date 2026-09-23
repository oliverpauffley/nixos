{
  flake.modules.homeManager.mac = { pkgs, ... }: {
    home.packages = with pkgs; [ pinentry_mac ];

    services.gpg-agent = {
      enable = true;
      enableSshSupport = true;
      pinentry.package = pkgs.pinentry_mac; # Requires pinentry-mac for macOS GUI prompts
    };
  };
  flake.modules.nixos.linux = { pkgs, ... }: {
    security.polkit.enable = true;

    environment.systemPackages = with pkgs; [
      pinentry-all
      pavucontrol
      polkit_gnome
    ];

    services.gnome.gnome-keyring.enable = true;
    security.pam.services.gdm.enableGnomeKeyring = true;
    # start gnome polkit
    systemd = {
      user.services.polkit-gnome-authentication-agent-1 = {
        description = "polkit-gnome-authentication-agent-1";
        wantedBy = [ "graphical-session.target" ];
        wants = [ "graphical-session.target" ];
        after = [ "graphical-session.target" ];
        serviceConfig = {
          Type = "simple";
          ExecStart = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
          Restart = "on-failure";
          RestartSec = 1;
          TimeoutStopSec = 10;
        };
      };
    };
  };
}
