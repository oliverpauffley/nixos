{
  flake.module.nixos.base = { pkgs }: {
    services.printing.enable = true;
    services.avahi = {
      enable = true;
      nssmdns4 = true;
      openFirewall = true;
    };
    services.printing.drivers = [
      pkgs.gutenprint
      pkgs.gutenprintBin
      pkgs.fxlinuxprint
      pkgs.foomatic-db-ppds-withNonfreeDb
    ];
  };
}
