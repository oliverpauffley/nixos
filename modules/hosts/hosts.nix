# Flake module that declares flake.hostsModules outputs and how to merge it
{ lib, flake-parts-lib, ... }:
let
  inherit (lib) mergeAttrs mkOption types;
  inherit (flake-parts-lib) mkSubmoduleOptions mkPerSystemOption;
  hostOptions = with lib;
    with types; {
      description = mkOption {
        type = str;
        default = null;
        description = ''
          host description
        '';
      };

      ipv4 = mkOption {
        type = str;
        description = ''
          own ipv4 address
        '';
      };

      dnsalias = mkOption {
        type = nullOr (listOf str);
        default = null;
        description = ''
          dnsalias for this host
        '';
      };

      isDNS = mkOption {
        type = bool;
        default = false;
        description = "Is this machine running core DNS";
      };
    };
in {
  # A custom flake submodule for defining a host
  options = {
    flake = mkSubmoduleOptions {
      hosts = mkOption rec {
        type = with types; attrsOf (submodule [{ options = hostOptions; }]);
        default = { };
        apply = mergeAttrs default;
        example = {
          "caladan" = {
            description = "homelab";
            ipv4 = "192.168.1.100";
            dnsalias = [ "caladan" "sonarr" "radarr" "dashboard" "plex" ];
          };
        };
        description = ''
          A collection of host information to allow for networking them together.
        '';
      };
    };

    perSystem = mkPerSystemOption {
      _file = ./hosts.nix;
      options = {
        currentHost = mkOption {
          type = with types; submodule [{ options = hostOptions; }];
          description = "The host that is described by this configuration";
        };
      };
    };
  };
}
