{
  flake.modules.nixos.base = { ... }: {
    services.xserver.xkb = {
      layout = "gb";
      options = "caps:ctrl_modifier";
    };
    environment.sessionVariables = {
      XDG_CACHE_HOME = "$HOME/.cache";
      XDG_CONFIG_HOME = "$HOME/.config";
      XDG_DATA_HOME = "$HOME/.local/share";
      XDG_STATE_HOME = "$HOME/.local/state";
    };
    hardware.graphics.enable = true;
    services.displayManager.gdm.enable = true;
    services.displayManager.gdm.wayland = true;

  };

  flake.modules.homeManager.base = { pkgs, config, ... }: {
    home.packages = with pkgs; [ firefox ];

    # default programs
    xdg.mimeApps = {
      enable = true;
      defaultApplications = {
        "application/pdf" = [ "zathura.desktop" "firefox.desktop" ];
        "image/png" = [ "feh.desktop" ];
        "text/plain" = [ "emacs.desktop" ];
        "x-scheme-handler/http" = [ "firefox.desktop" ];
        "x-scheme-handler/https" = [ "firefox.desktop" ];
        "x-scheme-handler/chrome" = [ "firefox.desktop" ];
        "text/html" = [ "firefox.desktop" ];
        "application/x-extension-htm" = [ "firefox.desktop" ];
        "application/x-extension-html" = [ "firefox.desktop" ];
        "application/x-extension-shtml" = [ "firefox.desktop" ];
        "application/xhtml+xml" = [ "firefox.desktop" ];
        "application/x-extension-xhtml" = [ "firefox.desktop" ];
        "application/x-extension-xht" = [ "firefox.desktop" ];
      };
    };
  };
}
