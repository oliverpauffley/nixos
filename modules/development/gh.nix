{
  flake.modules.homeManager.base = { pkgs, config, ... }: {
    programs.gh = {
      enable = true;
      gitCredentialHelper.enable = true;
    };
  };
}
