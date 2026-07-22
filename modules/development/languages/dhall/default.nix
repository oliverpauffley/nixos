{
  flake.modules.homeManager.dev = { pkgs, ... }: {
    home.packages = with pkgs; [
      dhall-lsp-server
      dhall
      dhall-json
      dhall-yaml
      dhall-docs
      dhall-nix
      dhallPackages.dhall-kubernetes
    ];
  };
}
