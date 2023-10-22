{
  config,
  options,
  pkgs,
  ...
}: let
  name = "Oliver Pauffley";
  maildir = "/home/ollie/.mail";
  personalEmail = "mrpauffley@gmail.com";
  workEmail = "opauffley@uw.co.uk";
  notmuchrc = "/home/ollie/.config/notmuch/notmuchrc";
in {
  home.packages = with pkgs; [isync];
  accounts.email = {
    maildirBasePath = "${maildir}";
    accounts = {
      Gmail = {
        address = "${personalEmail}";
        userName = "${personalEmail}";
        flavor = "gmail.com";
        passwordCommand = "op item get zu2yqk3edmu7rwhacvqqac2g2e --fields label=imap";
        primary = true;
        # gpg.encryptByDefault = true;
        mbsync = {
          enable = true;
          create = "both";
          expunge = "both";
          patterns = ["*" "[Gmail]*"]; # "[Gmail]/Sent Mail" ];
        };
        realName = "${name}";
      };
      Work = {
        address = "${workEmail}";
        userName = "${workEmail}";
        flavor = "gmail.com";
        passwordCommand = "op item get m6izfuxemokuy3vy6g65vlqyou --fields label=password";
        mbsync = {
          enable = true;
          create = "both";
          expunge = "both";
          patterns = ["*" "[Gmail]*"];
        };
        realName = "${name}";
        msmtp.enable = true;
      };
    };
  };

  programs = {
    msmtp.enable = true;
    mbsync.enable = true;
  };

  services = {
    mbsync = {
      enable = true;
      frequency = "*:0/15";
      preExec = "${pkgs.isync}/bin/mbsync -Ha";
      postExec = "${pkgs.mu}/bin/mu index -m ${maildir}";
    };
  };
}
