{
  flake.modules.homeManager.base = { pkgs, ... }: {
    home.packages = with pkgs; [ emacs-all-the-icons-fonts ];
    programs.emacs = {
      enable = true;
      package = pkgs.emacs;
      extraPackages = epkgs: with epkgs; [ vterm sqlite3 mu4e ];
    };
    services.emacs = {
      enable = true;
      client.enable = true;
    };
  };

}
