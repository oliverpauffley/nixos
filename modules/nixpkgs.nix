{ inputs, withSystem, ... }: {
  imports = [ inputs.pkgs-by-name-for-flake-parts.flakeModule ];
  perSystem = { system, ... }: {
    _module.args.pkgs = import inputs.nixpkgs {
      inherit system;
      config = { allowUnfreePredicate = _pkg: true; };
      overlays = [
        (final: _prev: {
          master = import inputs.nixpkgs-master {
            inherit (final) config;
            inherit system;
          };
        })
        (final: _prev: {
          unstable = import inputs.nixpkgs-unstable {
            inherit (final) config;
            inherit system;
          };
        })
      ];
    };
    pkgsDirectory = ../pkgs/by-name;
  };

  flake = {
    overlays.default = _final: prev:
      withSystem prev.stdenv.hostPlatform.system
      ({ config, ... }: { local = config.packages; });
  };
  flake.modules.homeManager.base = {
    config = {
      nix.settings.experimental-features = "nix-command flakes pipe-operators";
      nixpkgs.config = {
        allowUnfree = true;
        allowUnfreePredicate = _: true;
      };
    };
  };
  flake.modules.nixos.base = { nixpkgs.config.allowUnfree = true; };
}
