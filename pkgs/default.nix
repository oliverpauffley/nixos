# Custom packages, that can be defined similarly to ones from nixpkgs
# You can build them using 'nix build .#example' or (legacy) 'nix-build -A example'
{ pkgs ? (import ../nixpkgs.nix) { } }: {
  wiresteward = pkgs.callPackage ./wiresteward { };
  multi-gitter = pkgs.callPackage ./multi-gitter { };
  departure-mono = pkgs.callPackage ./departure_mono { };
  strongbox = pkgs.callPackage ./strongbox { };
  epson-thermal-printer-driver = pkgs.callPackage ./epson_thermal_printer_driver {};
}
