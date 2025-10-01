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
    # You can access packages and modules from different nixpkgs revs
    # at the same time. Here's an working example:
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    # Also see the 'unstable-packages' overlay at 'overlays/default.nix'.

    # For secrets
    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";

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
    templates.url = "github:the-nix-way/dev-templates";

  };

  outputs = inputs@{ self, nixpkgs, home-manager, nixos-hardware, rust-overlay
    , nix-colors, sops-nix, ... }:
    let
      inherit (self) outputs;
      forAllSystems = nixpkgs.lib.genAttrs [
        "aarch64-linux"
        "i686-linux"
        "x86_64-linux"
        "aarch64-darwin"
        "x86_64-darwin"
      ];
    in rec {
      # Your custom packages and modifications, exported as overlays
      overlays = import ./overlays { inherit inputs; };

      # Your custom packages
      # Acessible through 'nix build', 'nix shell', etc
      packages = forAllSystems (system:
        let pkgs = nixpkgs.legacyPackages.${system};
        in import ./pkgs { inherit pkgs; });
      # Devshell for bootstrapping
      # Acessible through 'nix develop' or 'nix-shell' (legacy)
      devShells = forAllSystems (system:
        let pkgs = nixpkgs.legacyPackages.${system};
        in import ./shell.nix { inherit pkgs; });

      # Reusable nixos modules you might want to export
      # These are usually stuff you would upstream into nixpkgs
      nixosModules = import ./modules/nixos;
      # Reusable home-manager modules you might want to export
      # These are usually stuff you would upstream into home-manager
      homeManagerModules = import ./modules/home-manager;

      colmena = {
        meta = {
          nixpkgs = import nixpkgs {
            system = "x86_64-linux";
            overlays = [
              # apply overlay to get unstable packages
              (overlays.unstable-packages)

            ];
          };
          specialArgs = { inherit inputs outputs packages; };
        };

        caladan = { name, nodes, pkgs, ... }: {
          deployment = {
            targetHost = "192.168.1.100";
            targetUser = "root";
          };
          imports = [
            ./hosts/caladan/configuration.nix
            inputs.sops-nix.nixosModules.sops
          ];
        };
      };

      # NixOS configuration entrypoint
      # Available through 'nixos-rebuild --flake .#your-hostname'
      nixosConfigurations = {
        arrakis = nixpkgs.lib.nixosSystem {
          specialArgs = { inherit inputs outputs; };
          modules = [
            # > Our main nixos configuration file <
            ./hosts/arrakis/configuration.nix
            home-manager.nixosModules.home-manager
            {
              home-manager.extraSpecialArgs = {
                inherit inputs outputs nix-colors;
              };
              home-manager.sharedModules =
                [ inputs.sops-nix.homeManagerModules.sops ];
              home-manager.backupFileExtension = "backup";
              home-manager.users.ollie = import ./home-manager/home.nix;
            }
            sops-nix.nixosModules.sops
          ];
        };

        # giedi-prime = nixpkgs.lib.nixosSystem {
        #   specialArgs = { inherit inputs outputs; };
        #   modules = [
        #     # > Our main nixos configuration file <
        #     ./hosts/geidi-prime/configuration.nix
        #     home-manager.nixosModules.home-manager
        #     {
        #       home-manager.extraSpecialArgs = {
        #         inherit inputs outputs nix-colors;
        #       };
        #       home-manager.sharedModules =
        #         [ inputs.sops-nix.homeManagerModules.sops ];
        #       home-manager.backupFileExtension = "backup";
        #       home-manager.users.ollie = import ./home-manager/home.nix;
        #     }
        #     sops-nix.nixosModules.sops
        #   ];
        # };
      };
    };
}
