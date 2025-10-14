{
  description = "ollie pauffley nix config";
  nixConfig = {
    extra-substituters =
      [ "https://nix-community.cachix.org" "https://cache.iog.io" ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
  };

  inputs = {
    # Nixpkgs
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
    nixpkgs-master.url = "github:NixOS/nixpkgs/master";
    nixpkgs-unstable.url = "github:/nixos/nixpkgs/nixpkgs-unstable";

    git-hooks.url = "github:cachix/git-hooks.nix";
    git-hooks.inputs.nixpkgs.follows = "nixpkgs";

    # flake parts for modular configuration
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";
    # import all modules
    import-tree.url = "github:vic/import-tree";
    # make my own packages
    pkgs-by-name-for-flake-parts.url =
      "github:drupol/pkgs-by-name-for-flake-parts";

    # For secrets
    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";

    nixos-generators.url = "github:nix-community/nixos-generators";
    nixos-generators.inputs.nixpkgs.follows = "nixpkgs";

    # my secrets repo
    mysecrets = {
      url =
        "git+ssh://git@github.com/oliverpauffley/nix-secrets.git?shallow=1&ref=main";
      flake = false;
    };

    # Home manager
    home-manager.url = "github:nix-community/home-manager/release-25.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    hardware.url = "github:nixos/nixos-hardware";

    # reproducible rust
    rust-overlay.url = "github:oxalica/rust-overlay";

    nix-colors.url = "github:misterio77/nix-colors";
    treefmt-nix.url = "github:numtide/treefmt-nix";

    # all systems
    systems.url = "github:nix-systems/default";
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; }
    (inputs.import-tree ./modules);
  # outputs = inputs@{ flake-parts, nixos-hardware, rust-overlay, home-manager
  #   , nix-colors, templates, sops-nix, mysecrets, nixpkgs, ... }:
  #   flake-parts.lib.mkFlake { inherit inputs; }
  #   (top@{ config, withSystem, moduleWithSystem, ... }: {
  #     systems = [
  #       "aarch64-linux"
  #       "i686-linux"
  #       "x86_64-linux"
  #       "aarch64-darwin"
  #       "x86_64-darwin"
  #     ];
  #     flake = { config, pkgs, ... }: rec {
  #       # TODO a bootstrap shell?
  #       # imports = [ ./shell.nix ];
  #       # Your custom packages and modifications, exported as overlays
  #       overlays = import ./overlays { inherit inputs; };
  #       # Reusable nixos modules you might want to export
  #       # These are usually stuff you would upstream into nixpkgs
  #       nixosModules = import ./modules/nixos;
  #       # Reusable home-manager modules you might want to export
  #       # These are usually stuff you would upstream into home-manager
  #       homeManagerModules = import ./modules/home-manager;

  #       colmena = {
  #         meta = {
  #           nixpkgs = import inputs.nixpkgs {
  #             system = "x86_64-linux";
  #             overlays = [
  #               # apply overlay to get unstable packages
  #               overlays.unstable-packages

  #             ];
  #           };
  #           specialArgs = { inherit inputs; };
  #         };

  #         caladan = { name, nodes, pkgs, ... }: {
  #           deployment = {
  #             targetHost = "192.168.1.100";
  #             targetUser = "root";
  #           };
  #           imports = [
  #             ./hosts/caladan/configuration.nix
  #             inputs.sops-nix.nixosModules.sops
  #           ];
  #         };
  #       };

  #       # NixOS configuration entrypoint
  #       # Available through 'nixos-rebuild --flake .#your-hostname'
  #       nixosConfigurations = {
  #         arrakis = inputs.nixpkgs.lib.nixosSystem {
  #           specialArgs = { inherit inputs; };
  #           modules = [
  #             # > Our main nixos configuration file <
  #             ./hosts/arrakis/configuration.nix
  #             inputs.home-manager.nixosModules.home-manager
  #             {
  #               home-manager = {
  #                 extraSpecialArgs = { inherit inputs; };
  #                 sharedModules = [ inputs.sops-nix.homeManagerModules.sops ];
  #                 backupFileExtension = "backup";
  #                 users.ollie = import ./home-manager/home.nix;
  #               };
  #             }
  #             inputs.sops-nix.nixosModules.sops
  #           ];
  #         };

  #         giedi-prime = inputs.nixpkgs.lib.nixosSystem {
  #           specialArgs = { inherit inputs; };
  #           modules = [
  #             # > Our main nixos configuration file <
  #             ./hosts/giedi-prime/configuration.nix
  #             inputs.home-manager.nixosModules.home-manager
  #             {
  #               home-manager = {
  #                 extraSpecialArgs = { inherit inputs; };
  #                 sharedModules = [ inputs.sops-nix.homeManagerModules.sops ];
  #                 backupFileExtension = "backup";
  #                 users.ollie = import ./home-manager/home.nix;
  #               };
  #             }
  #             inputs.sops-nix.nixosModules.sops
  #           ];
  #         };

  #       };
  #     };
  #   });
}
