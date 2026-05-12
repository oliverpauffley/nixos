{
  flake.modules.nixos.base = { config, ... }: {
    nix = {
      envVars = {
        NIX_GITHUB_PRIVATE_USERNAME = config.sops.secrets.github_username.path;
        NIX_GITHUB_PRIVATE_PASSWORD = config.sops.secrets.github_token.path;
      };
    };
  };
  flake.modules.homeManager.base = { pkgs, config, ... }: {
    home.packages = with pkgs; [
      local.strongbox
      local.gomerge
      libsecret
      ssh-askpass-fullscreen
    ];
    programs.git = {
      enable = true;
      lfs.enable = true;
      attributes = [ "go.mod linguist-generated" "go.sum linguist-generated" ];
      ignores = [ ".envrc" ".direnv/*" ];
      signing = {
        signByDefault = true;
        key = "898E9AF3BA558BBD27CCEC76776333D265A54BED";
      };
      settings = {
        user = {
          email = "mrpauffley@gmail.com";
          name = "oliverpauffley";
        };
        github.user = "oliverpauffley";
        credential.helper = "${
            pkgs.git.override { withLibsecret = true; }
          }/bin/git-credential-libsecret";
        init.defaultBranch = "main";
        url."git@github.com:".insteadOf = "https://github.com/";
        filter.strongbox = {
          clean = "strongbox -clean %f";
          smudge = "strongbox -smudge %f";
          required = "true";
        };
        diff.strongbox.textconv = "strongbox -diff";
        merge.conflictStyle = "diff3";
        merge.strongbox.driver =
          "strongbox -merge-file %O -merge-file %A -merge-file %B -merge-file %L -merge-file %P -merge-file %S -merge-file %X -merge-file %Y";
      };
    };

    programs.gh = {
      enable = true;
      gitCredentialHelper.enable = true;
    };

    # better merge conflicts
    programs.mergiraf.enable = true;
  };
}
