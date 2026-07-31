{ inputs, withSystem, ... }: {
  imports = [ inputs.pkgs-by-name-for-flake-parts.flakeModule ];
  flake = {
    overlays.default =
      final: prev:
      withSystem prev.stdenv.hostPlatform.system (
        { config, ... }: {
          local = config.packages;
          unstable = import inputs.nixpkgs-unstable {
            inherit (final.stdenv.hostPlatform) system;
            inherit (final) config;
          };
          pnpm_9_15_9 = final.pnpm_10;
          pnpm_10_29_2 = final.pnpm_10;
        }
      );
  };

  perSystem = { system, ... }: {
    _module.args.pkgs = import inputs.nixpkgs {
      inherit system;
      config.allowUnfree = true;
      overlays = [
        inputs.self.overlays.default
      ];
    };
    pkgsDirectory = ../../pkgs/by-name;
  };

  flake.modules.homeManager.base = {
    config = {
      nix.settings.experimental-features = "nix-command flakes";
      nixpkgs.config.allowUnfree = true;
      nixpkgs.overlays = [
        inputs.self.overlays.default
      ];
    };
  };
  flake.modules.nixos.base = { inputs, ... }: {
    nixpkgs.overlays = [
      inputs.self.overlays.default
    ];
    nixpkgs.config.allowUnfree = true;
    nix.settings.experimental-features = [
      "nix-command"
      "flakes"
    ];
  };
  flake.modules.darwin.base = { inputs, ... }: {
    nixpkgs.overlays = [
      inputs.self.overlays.default
    ];
    nixpkgs.config.allowUnfree = true;
    nix.settings.experimental-features = [
      "nix-command"
      "flakes"
    ];
  };

  # homeManager.base pulls in every home-manager module tagged "base" across
  # the repo, several of which are Linux-only (udiskie, xdg.mimeApps, ...).
  # darwin hosts use this instead rather than trying to filter that down.
  flake.modules.homeManager.darwin = {
    config = {
      nix.settings.experimental-features = "nix-command flakes";
      nixpkgs.config.allowUnfree = true;
      nixpkgs.overlays = [
        inputs.self.overlays.default
      ];
    };
  };
}
