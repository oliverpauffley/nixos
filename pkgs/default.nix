# Custom packages, that can be defined similarly to ones from nixpkgs
# You can build them using 'nix build .#example' or (legacy) 'nix-build -A example'
{ pkgs ? (import ../nixpkgs.nix) { } }: {
  wiresteward = pkgs.callPackage ./wiresteward { };
  strongbox = pkgs.callPackage ./strongbox { };
  my-cookies = pkgs.callPackage ./my_cookies {
    buildPythonApplication = pkgs.python39Packages.buildPythonApplication;
    browser-cookie3 = pkgs.python39Packages.browser-cookie3;
  };
}
