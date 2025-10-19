{
  flake.modules.nixos.base = { pkgs, ... }: {
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
      displayManager.gdm.enable = true;
      displayManager.gdm.wayland = false;
      desktopManager = { xterm.enable = false; };
    };

  };
  flake.modules.homeManager.base = { pkgs, config, ... }: {
    services.picom.enable = true;
    home.packages = with pkgs; [ firefox feh arandr ];

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
