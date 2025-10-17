{
  flake.modules.homeManager.base = { pkgs, inputs, ... }: {
    nixpkgs.overlays = [ inputs.self.overlays.default ];
    home.packages = with pkgs; [
      jq
      rq
      gnuplot
      fd
      ripgrep
      eza
      bat
      pandoc
      util-linux
      xan # process csvs from shell
      xclip
      procs # instead of ps
      imagemagick
      zathura
      pkg-config
      dnsutils
      eclint
      just
      mermaid-cli
      vlc
      unrar
      comma # run commands that aren't installed with ","
      local.multi-gitter
      btop
      calibre # ebooks
      exercism
    ];
  };
}
