{ config, options, pkgs, ... }:
let
  name = "Oliver Pauffley";
  maildir = "/home/ollie/.mail";
  personalEmail = "mrpauffley@gmail.com";
  notmuchrc = "/home/ollie/.config/notmuch/notmuchrc";
in {
  # overlay for oauth
  nixpkgs.overlays = [
    (_: super: {
      isync = super.isync.override { withCyrusSaslXoauth2 = true; };
    })
  ];
  home.packages = with pkgs; [
    # oama for refresh tokens
    oama
    isync
  ];
  accounts.email = {
    maildirBasePath = "${maildir}";
    accounts = {
      Gmail = {
        address = "${personalEmail}";
        userName = "${personalEmail}";
        flavor = "gmail.com";
        passwordCommand = "${pkgs.oama}/bin/oama access ${personalEmail}";
        primary = true;
        mbsync = {
          enable = true;
          create = "both";
          expunge = "both";
          extraConfig.account = { AuthMechs = "XOAUTH2"; };
          patterns = [ "*" "[Gmail]*" ]; # "[Gmail]/Sent Mail" ];
        };
        realName = "${name}";
      };
    };
  };

  programs = {
    mbsync.enable = true;
    msmtp.enable = true;
  };
}
