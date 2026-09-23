{
  flake.modules.homeManager.dev =
    {
      pkgs,
      lib,
      inputs,
      ...
    }:
    {
      home.packages = with pkgs; [
        # haskell
        stack
        cabal-install
        zlib
        (haskellPackages.ghcWithPackages (
          hpkgs:
          lib.optionals pkgs.stdenv.hostPlatform.isLinux [
            hpkgs.xmobar
            hpkgs.xmonad
            hpkgs.xmonad-contrib
          ]
          ++ [
            hpkgs.random
            hpkgs.scotty
            hpkgs.zlib
            hpkgs.criterion
            hpkgs.vector
            hpkgs.containers
            hpkgs.text-show
            hpkgs.cabal2nix
            hpkgs.doctest
            hpkgs.fourmolu
            hpkgs.quickcheck-classes
          ]
        ))
        haskellPackages.hoogle
        haskellPackages.ghcide
        haskell-language-server
        stylish-haskell
        ghcid
        nix-prefetch-git
        niv
        haskellPackages.threadscope
      ];
    };
}
