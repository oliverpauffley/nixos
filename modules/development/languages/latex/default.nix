{
  flake.modules.homeManager.dev = { pkgs, ... }: {
    home.packages = with pkgs; [ texlive.combined.scheme-full ];
  };
}
