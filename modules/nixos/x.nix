{ config, lib, pkgs, ... }:

let cfg = config.modules.x;
in {
  options.modules.x = with lib; {
    enable = lib.mkEnableOption "Enable the x11 server";
  };
  config = lib.mkIf cfg.enable {

    programs._1password-gui.enable = true;
    programs._1password-gui.polkitPolicyOwners = [ "ollie" ];
    environment.sessionVariables = {
      XDG_CACHE_HOME = "$HOME/.cache";
      XDG_CONFIG_HOME = "$HOME/.config";
      XDG_DATA_HOME = "$HOME/.local/share";
      XDG_STATE_HOME = "$HOME/.local/state";
    };

    hardware.graphics.enable = true;
    services.xserver = {
      enable = true;
      xkb = {
        layout = "gb";
        options = "caps:ctrl_modifier";
      };
      dpi = 180;
      upscaleDefaultCursor = true;
      videoDrivers = [ "displaylink" "modesetting" ];
      displayManager.gdm.enable = true;
      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };

      desktopManager = { xterm.enable = false; };
    };

    # start gnome polkit
    systemd = {
      user.services.polkit-gnome-authentication-agent-1 = {
        description = "polkit-gnome-authentication-agent-1";
        wantedBy = [ "graphical-session.target" ];
        wants = [ "graphical-session.target" ];
        after = [ "graphical-session.target" ];
        serviceConfig = {
          Type = "simple";
          ExecStart =
            "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
          Restart = "on-failure";
          RestartSec = 1;
          TimeoutStopSec = 10;
        };
      };
    };

    services.gnome.gnome-keyring.enable = true;
    security.pam.services.gdm.enableGnomeKeyring = true;

    # printing
    services.printing.enable = true;
    services.printing.drivers = [
      (pkgs.callPackage ../../pkgs/epson_thermal_printer_driver { })
      pkgs.gutenprint
      pkgs.gutenprintBin
      pkgs.fxlinuxprint
      pkgs.foomatic-db-ppds-withNonfreeDb
    ];
    # tablet
    hardware.opentabletdriver = {
      enable = true;
      daemon.enable = true;
    };
    environment.systemPackages = with pkgs; [ firefox feh arandr ];
  };
}
