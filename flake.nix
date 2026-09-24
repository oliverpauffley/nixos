{
  description = "ollie pauffley nix config";

  inputs = {
    # Nixpkgs
    nixpkgs.url = "github:nixos/nixpkgs/nixos-26.05";

    git-hooks.url = "github:cachix/git-hooks.nix";
    git-hooks.inputs.nixpkgs.follows = "nixpkgs";

    nix-darwin.url = "github:nix-darwin/nix-darwin/nix-darwin-26.05";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";

    # flake parts for modular configuration
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";
    # import all modules
    import-tree.url = "github:vic/import-tree";
    import-tree.inputs.nixpkgs.follows = "nixpkgs";

    # make my own packages
    pkgs-by-name-for-flake-parts.url = "github:drupol/pkgs-by-name-for-flake-parts";

    # For secrets
    #sops-nix.url = "github:Mic92/sops-nix";
    #sops-nix.inputs.nixpkgs.follows = "nixpkgs";

    nixos-generators.url = "github:nix-community/nixos-generators";
    nixos-generators.inputs.nixpkgs.follows = "nixpkgs";

    # my secrets repo
    #mysecrets = {
    # url = "git+ssh://git@github.com/oliverpauffley/nix-secrets.git?shallow=1&ref=main";
    # flake = false;
    #};

    # Home manager
    home-manager.url = "github:nix-community/home-manager/release-26.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    nix-colors.url = "github:misterio77/nix-colors";
    nix-colors.inputs.nixpkgs.follows = "nixpkgs";

    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";

    noctalia = {
      url = "github:noctalia-dev/noctalia-shell";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Spoons!
    paperWM = {
      url = "github:mogenson/paperWM.spoon";
      flake = false;
    };

    warpMouse = {
      url = "github:mogenson/WarpMouse.spoon";
      flake = false;
    };

    # all systems
    systems.url = "github:nix-systems/default";
  };

  outputs = inputs: inputs.flake-parts.lib.mkFlake { inherit inputs; } (inputs.import-tree ./modules);
}
