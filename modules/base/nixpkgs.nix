{ inputs, withSystem, ... }: {
  imports = [ inputs.pkgs-by-name-for-flake-parts.flakeModule ];
  perSystem = { system, ... }: {
    _module.args.pkgs = import inputs.nixpkgs {
      inherit system;
      config.allowUnfree = true;
      overlays = [
        (final: _prev: {
          unstable = import inputs.nixpkgs-unstable {
            inherit (final) config;
            inherit system;
          };
        })
      ];
    };
    pkgsDirectory = ../../pkgs/by-name;
  };

  flake = {
    overlays.default = _final: prev:
      withSystem prev.stdenv.hostPlatform.system
      ({ config, ... }: { local = config.packages; });
  };
  flake.modules.homeManager.base = {
    config = {
      nix.settings.experimental-features = "nix-command flakes";
      nixpkgs.config.allowUnfree = true;
    };
  };
  flake.modules.nixos.base = { inputs, ... }: {
    nixpkgs.overlays = [
      (final: _prev: {
        unstable = import inputs.nixpkgs-unstable {
          system = final.stdenv.hostPlatform.system;
          config.allowUnfree = true;
        };
      })
    ];
    nixpkgs.config.allowUnfree = true;
    nix.settings.experimental-features = [ "nix-command" "flakes" ];
  };
}
