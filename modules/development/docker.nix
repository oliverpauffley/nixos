{
  flake.modules.nixos.dev = { pkgs, ... }: {
    virtualisation.docker.enable = true;
  };

  flake.modules.homeManager.dev = { pkgs, ... }: {
    home.packages = with pkgs; [ dive ];
  };
}
