{
  flake.modules.homeManager.dev = { pkgs, ... }: {
    home.packages = with pkgs; [
      gcc
      go
      go-outline
      gocode-gomod
      godef
      golint
      gomodifytags
      gopkgs
      gopls
      gore
      gotests
      gotestsum
      golangci-lint
      gotools
      gofumpt
      pkgs.local.moq
      pkgs.local.mockgen
      sqlc
      graphviz
    ];
  };
}
