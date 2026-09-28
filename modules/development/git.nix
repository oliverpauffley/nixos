{
  flake.modules.nixos.base = { config, ... }: {
  };
  flake.modules.homeManager.base = { pkgs, config, ... }: {
    home.packages = with pkgs; [
      local.gomerge
      libsecret
      #ssh-askpass-fullscreen
    ];
    programs.git = {
      enable = true;
      lfs.enable = true;
      attributes = [
        "go.mod linguist-generated"
        "go.sum linguist-generated"
      ];
      ignores = [
        ".envrc"
        ".direnv/*"
      ];
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
        credential.helper = "${pkgs.git.override { withLibsecret = true; }}/bin/git-credential-libsecret";
        init.defaultBranch = "main";
        url."git@github.com:".insteadOf = "https://github.com/";
        merge.conflictStyle = "diff3";
      };
    };

    programs.gh = {
      enable = true;
      gitCredentialHelper.enable = true;
    };

    # better merge conflicts
    programs.mergiraf = {
      enable = true;
      enableGitIntegration = true;
    };
  };
}
