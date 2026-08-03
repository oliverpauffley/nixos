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
    services.displayManager.ly = {
      enable = true;
      settings = {
        animation = "matrix";
        bigclock = "none";
        brightness_down_key = "null";
        brightness_up_key = "null";
        clear_password = true;
        default_input = "password";
        hide_version_string = true;
      };
    };

  };

  flake.modules.homeManager.base = { pkgs, config, ... }: {
    home.packages = with pkgs; [ firefox ];
  };

  flake.modules.homeManager.linux = {
    # default programs
    xdg.mimeApps = {
      enable = true;
      defaultApplications = {
        "application/pdf" = [
          "zathura.desktop"
          "firefox.desktop"
        ];
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
