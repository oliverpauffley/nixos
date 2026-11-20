{
  flake.modules.homeManager.dev = { pkgs, ... }: {
    home.packages = with pkgs; [
      typescript
      typescript-language-server
    ];
  };
}
