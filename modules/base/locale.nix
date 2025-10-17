{
  flake.modules.nixos.base = {
    i18n.defaultLocale = "en_GB.UTF-8";
    console = {
      font = "Lat2-Terminus16";
      useXkbConfig = true; # use xkbOptions in tty.
    };
    time.timeZone = "Europe/London";
  };
}
