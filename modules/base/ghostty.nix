{
  flake.modules.homeManager.base = { config, ... }: {
    programs.ghostty = {
      enableFishIntegration = true;
      installVimSyntax = true;
      settings = {
        font-size = 14;
        font-family = config.fontProfiles.regular.family;
        copy-on-select = true;
      };
      enable = true;
    };
  };
}
