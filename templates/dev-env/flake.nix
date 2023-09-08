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
        packages = [
          # go 1.20 (specified by overlay)
          pkgs.go

          # goimports, godoc, etc.
          pkgs.gotools

          # https://github.com/golangci/golangci-lint
          pkgs.golangci-lint

          # fancy way of handling migrations
          tusker

          # for protos
          pkgs.buf
        ];
        dockerImage = pkgs.dockerTools.buildImage {
          name = "devenv";
          tag = "latest";
          copyToRoot = [ packages ];
          config = { Cmd = [ "usr/bin/nu" ]; };
        };
      in with pkgs; {
        devShells.default = mkShell { inputsFrom = [ packages ]; };
        packages = { inherit dockerImage; };
      });
}
