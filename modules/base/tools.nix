{
  flake.modules.nixos.base = { pkgs, ... }: {
    environment.systemPackages = with pkgs; [
      fd
      vlc
    ];

  };
  flake.modules.homeManager.base = { pkgs, inputs, ... }: {
    nixpkgs.overlays = [ inputs.self.overlays.default ];
    home.packages =
      with pkgs;
      [
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
        gifsicle
        #zathura
        pkg-config
        dnsutils
        eclint
        just
        mermaid-cli
        unrar
        comma # run commands that aren't installed with ","
        local.multi-gitter
        btop
        exercism
        claude-code
        claude-agent-acp
        act # github actions locally
        dust
        gnupg
        postgresql.pg_config
      ]
      ++ lib.optionals stdenv.hostPlatform.isLinux [
        vlc
        calibre
      ]
      ++ lib.optionals stdenv.hostPlatform.isDarwin [ pngpaste ];
  };
}
