{
  flake.modules.homeManager.base = { pkgs, ... }: {
    programs.zoxide = {
      enable = true;
      options = [
        "--cmd cd"
      ];
    };
  };
}
