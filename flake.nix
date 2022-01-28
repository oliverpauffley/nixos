{
  description = "System configuration";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    emacsOverlay.url = "github:nix-community/emacs-overlay";
    emacsOverlay.inputs.nixpkgs.follows = "nixpkgs";
    nur.url = "github:nix-community/NUR";
  };

  outputs = { self, nixpkgs, home-manager, emacsOverlay, nur }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config = {
          allowUnfree = true;
        };
      };
      lib = nixpkgs.lib;
      overlays = [
        emacsOverlay.overlay
        nur.overlay
      ];
    in
    {
      homeManagerConfigurations = {
        ollie = home-manager.lib.homeManagerConfiguration {
          inherit system pkgs;
          username = "ollie";
          homeDirectory = "/home/ollie";
          configuration = {
            nixpkgs = {
              inherit overlays;
              config.allowUnfree = true;
            };
            imports = [
              ./home-manager/home.nix
            ];
          };
        };
      };
      nixosConfigurations = {
        nixospc = lib.nixosSystem {
          inherit system;
          modules = [
            ./configuration.nix
          ];
        };
      };
    };
}
