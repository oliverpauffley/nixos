{ inputs, withSystem, ... }:

let
  overlays = [
    inputs.self.overlays.default
  ];

  nixpkgsConfig = {
    allowUnfree = true;
  };

  nixSettings = {
    substituters = [
      "https://cache.nixos.org"
      "https://rqube.cachix.org"
      "https://cache.iog.io"
    ];

    trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "rqube.cachix.org-1:POl2bnMMKa9/iw4KKBQHr0iysHG/iKOnHN62UMyxNxI="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
  };

  experimentalFeatures = [
    "nix-command"
    "flakes"
  ];
in
{
  imports = [
    inputs.pkgs-by-name-for-flake-parts.flakeModule
  ];

  flake = {
    overlays.default =
      final: prev:
      withSystem prev.stdenv.hostPlatform.system (
        { config, ... }: {
          local = config.packages;
        }
      );
  };

  perSystem = { system, ... }: {
    _module.args.pkgs = import inputs.nixpkgs {
      inherit system;
      config = nixpkgsConfig;
      inherit overlays;
    };

    pkgsDirectory = ../../pkgs/by-name;
  };

  # ---------------------------------------------------------------------------
  # Home Manager
  # ---------------------------------------------------------------------------

  flake.modules.homeManager.base = {
    nixpkgs = {
      config = nixpkgsConfig;
      inherit overlays;
    };

    nix.settings = nixSettings // {
      experimental-features = experimentalFeatures;
      trusted-users = [ "ollie" ];
    };
  };

  # ---------------------------------------------------------------------------
  # NixOS
  # ---------------------------------------------------------------------------

  flake.modules.nixos.base = {
    nixpkgs = {
      config = nixpkgsConfig;
      inherit overlays;
    };

    nix.settings = nixSettings // {
      experimental-features = experimentalFeatures;
      ssl-cert-file = /etc/ssl/cert.pem;
    };
  };

  # ---------------------------------------------------------------------------
  # nix-darwin
  # ---------------------------------------------------------------------------

  flake.modules.darwin.base = {
    nixpkgs = {
      config = nixpkgsConfig;
      inherit overlays;
    };

    nix.settings = nixSettings // {
      experimental-features = experimentalFeatures;
    };
  };
}
