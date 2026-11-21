{
  flake.modules.homeManager.base = { pkgs, config, ... }: {
    programs.mergiraf = {
      enable = true;
      enableGitIntegration = true;
    };
  };
}
