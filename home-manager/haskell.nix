{ pkgs, ... }: {
  home.packages = with pkgs; [
    # haskell
    stack
    cabal-install
    zlib
    (haskellPackages.ghcWithPackages (hpkgs: [
      hpkgs.xmobar
      hpkgs.xmonad
      hpkgs.xmonad-contrib
      hpkgs.random
      hpkgs.scotty
      hpkgs.zlib
      hpkgs.criterion
      hpkgs.vector
      hpkgs.record-dot-preprocessor
    ]))
    haskellPackages.haskell-language-server
    haskellPackages.hoogle
    haskellPackages.ghcide
    stylish-haskell
    ghcid
  ];
}
