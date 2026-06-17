{
  flake.modules.homeManager.base = { config, ... }: {
    programs.ghostty = {
      enableFishIntegration = true;
      installVimSyntax = true;
      settings = { font-size = 10; };
      enable = true;
    };
  };
}
