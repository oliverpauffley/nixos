{
  inputs = {
    nixpkgs = { url = "github:nixos/nixpkgs/nixos-unstable"; };
    flake-utils = { url = "github:numtide/flake-utils"; };

  };
  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        goVersion = 20; # Change this to update the whole stack
        overlays =
          [ (final: prev: { go = prev."go_1_${toString goVersion}"; }) ];
        pkgs = nixpkgs.legacyPackages.${system};
        tusker = import ./tusker.nix { inherit pkgs; };
      in with pkgs; {
        devShells.default = mkShell {
          packages = [
            # go 1.20 (specified by overlay)
            go

            # goimports, godoc, etc.
            gotools

            # https://github.com/golangci/golangci-lint
            golangci-lint

            # fancy way of handling migrations
            tusker

            # for protos
            buf
          ];
        };
      });
}
