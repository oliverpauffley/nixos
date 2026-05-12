{
  flake.modules.homeManager.dev = { pkgs, ... }: {
    home.packages = with pkgs.elmPackages; [
      elm
      elm-format
      elm-test
      elm-language-server
    ];
  };
}
