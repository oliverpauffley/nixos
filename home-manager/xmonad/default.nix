{ config, pkgs, lib, ... }: {
  xsession = {
    enable = true;

    windowManager.xmonad =

      let
        xmonadConfig = pkgs.writeTextFile {
          name = "xmonad-config";
          text = builtins.readFile ./config.hs;
        };
      in {
        enable = true;
        enableContribAndExtras = true;
        extraPackages = hp: [ hp.dbus hp.monad-logger hp.xmonad-contrib ];
        config = xmonadConfig;
      };
  };
}
