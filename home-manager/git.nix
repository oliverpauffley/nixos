{ config, lib, pkgs, ... }: {
  home.packages = with pkgs; [ strongbox libsecret ssh-askpass-fullscreen ];
  programs.git = {
    enable = true;
    lfs.enable = true;
    userEmail = "mrpauffley@gmail.com";
    userName = "oliverpauffley";
    attributes = [ "go.mod linguist-generated" "go.sum linguist-generated" ];
    ignores = [ ".envrc" ".direnv/*" ];
    extraConfig = {
      github.user = "oliverpauffley";
      credential.helper = "${
          pkgs.git.override { withLibsecret = true; }
        }/bin/git-credential-libsecret";
      filter = {
        strongbox = {
          clean = "strongbox -clean %f";
          smudge = "strongbox -smudge %f";
          required = "true";
        };
      };
      diff = { strongbox = { textconv = "strongbox -diff"; }; };
      init = { defaultBranch = "main"; };
      url = { "git@github.com:" = { insteadOf = "https://github.com/"; }; };
      gpg."ssh".program = "${pkgs._1password-gui}/bin/op-ssh-sign";
    };
    signing = {
      signByDefault = true;
      key = "898E9AF3BA558BBD27CCEC76776333D265A54BED";
    };
  };

  programs.gh = {
    enable = true;
    gitCredentialHelper.enable = true;
  };
}
