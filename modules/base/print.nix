{
  flake.module.nixos.base = { pkgs, lib }: {
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
    # hardware = let
    #   brother = "Brother_HL-3170CDW_NixOS";
    #   hostName = "printer.home.hoeg.com";
    # in {
    #   printers = {
    #     ensureDefaultPrinter = brother;
    #     ensurePrinters = [{
    #       name = brother;
    #       deviceUri = "ipp://${hostName}/ipp";
    #       model = "everywhere";
    #       description = lib.replaceStrings [ "_" ] [ " " ] brother;
    #       location = "Study";
    #     }];
    #   };
    # };
  };
}
