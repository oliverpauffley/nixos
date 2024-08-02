{ pkgs, ... }: {
  programs.emacs = {
    enable = true;
    package = pkgs.emacs;
    extraPackages = epkgs: with epkgs; [ vterm sqlite3 ];
  };

  services.emacs = {
    enable = true;
    client.enable = true;
    client.arguments = [ "-nc" ];
    startWithUserSession = "graphical";
    socketActivation.enable = true;
  };
}
