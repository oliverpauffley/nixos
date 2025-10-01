{ config, lib, pkgs, ... }:
let cfg = config.modules.work;
in {
  imports = [ ../../services/wiresteward ];
  options.modules.work = with lib; {
    enable = lib.mkEnableOption "Enable the work module";
  };

  config = lib.mkIf cfg.enable {
    networking.extraHosts = "10.91.9.5 webappint.tp.private";
    services.wiresteward.enable = true;
    environment.systemPackages = with pkgs; [ strongbox ];
  };

}
