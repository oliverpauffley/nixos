{
  flake.modules.homeManager.dev = { pkgs, ... }: {
    home.packages = with pkgs; [
      postgresql

      sqlite
      sqlitecpp
      mariadb
    ];
  };
}
