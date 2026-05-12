{
  flake.modules.homeManager.dev = { pkgs, config, ... }: {
    home.packages = with pkgs; [ kubectl kustomize ];
  };
}
