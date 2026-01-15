{
  flake.modules.homeManager.dev = { pkgs, inputs, ... }: {
    home.packages = with pkgs; [ janet jpm ];
  };
}
