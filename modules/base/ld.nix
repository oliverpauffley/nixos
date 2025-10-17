{
  flake.modules.nixos.base = { pkgs, ... }: {
    # patch downloaded binaries
    programs.nix-ld.enable = true;

    # Sets up all the libraries to load
    programs.nix-ld.libraries = with pkgs; [
      stdenv.cc.cc
      zlib
      fuse3
      icu
      nss
      openssl
      curl
      expat
      # ...
    ];
  };
}
