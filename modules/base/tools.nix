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
        xclip
        procs # instead of ps
        #zathura
        dnsutils
        unrar
        comma # run commands that aren't installed with ","
        btop
        act # github actions locally
        dust
        gnupg
      ]
      ++ lib.optionals stdenv.hostPlatform.isDarwin [ pngpaste ];
  };
}
