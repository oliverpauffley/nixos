{ config, lib, pkgs, ... }:

{
  # git
  programs.git = {
    enable = true;
    userEmail = "mrpauffley@gmail.com";
    userName = "Ollie";
    attributes = [ "go.mod linguist-generated" "go.sum linguist-generated" ];
    extraConfig = {
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
    };
    signing = {
      signByDefault = true;
      key = "13C3AC686C7499A76DF6CD55E0BFCBFE7ED19B38";
    };
  };

}
