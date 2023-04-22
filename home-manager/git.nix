{ config, lib, pkgs, ... }:
{
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
      key = "898E9AF3BA558BBD27CCEC76776333D265A54BED";
    };
  };

}
