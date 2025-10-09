{ config, lib, ... }:
let cfg = config.modules.laptop;
in {
  options.modules.laptop = {
    enable = lib.mkEnableOption "Enable the laptop module";
    networkInterface = lib.mkOption {
      type = lib.types.str;
      description = "networking interface (for captive-browser setup)";
    };
  };
  config = lib.mkIf cfg.enable {
    # browser specifically for getting through network logins
    programs.captive-browser.enable = true;
    programs.captive-browser.interface = cfg.networkInterface;

    # bluetooth
    hardware.bluetooth.enable = true;
    services.blueman.enable = true;

    # touchpads
    services.libinput = {
      enable = true;

      touchpad = {
        clickMethod = "buttonareas";
        disableWhileTyping = true;
        middleEmulation = true;
        tapping = true;
        additionalOptions = ''
          Option "PalmDetection" "on"
          Option "TappingButtonMap" "lmr"
        '';
      };
    };

    # power managment
    services.upower.enable = true;
    services.tlp = {
      enable = true;
      settings = {
        CPU_SCALING_GOVERNOR_ON_AC = "performance";
        CPU_SCALING_GOVERNOR_ON_BAT = "powersave";
      };
    };
  };
}
