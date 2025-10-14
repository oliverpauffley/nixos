{
  flake.modules.homeManager.dev = { pkgs, ... }: {
    home.packages = with pkgs; [ nixfmt-classic nil statix ];

  };
}
