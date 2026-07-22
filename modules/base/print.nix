{
  flake.modules.nixos.base = { pkgs, lib, ... }: {
    services.printing = {
      enable = true;
      drivers = with pkgs; [
        cups-filters
        cups-browsed
        pkgs.gutenprint
        pkgs.gutenprintBin
        pkgs.fxlinuxprint
        pkgs.foomatic-db-ppds-withNonfreeDb

      ];
    };
    services.avahi = {
      enable = true;
      nssmdns4 = true;
      openFirewall = true;
    };
  };
}
