{
  flake.modules.homeManager.dev = { pkgs, inputs, ... }: {
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
      moq
      sqlc
      #structurizr-cli
      graphviz
    ];

  };
}
