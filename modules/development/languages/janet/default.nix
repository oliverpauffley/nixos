{
  flake.modules.homeManager.dev = { pkgs, inputs, ... }: {
    home.packages = [ pkgs.janet pkgs.jpm pkgs.local.janet-lsp ];
  };
}
