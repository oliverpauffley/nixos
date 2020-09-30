{ pkgs, config, ... }:

{
  accounts.email = {
    maildirBasePath = "/home/ollie/Mail";
    accounts.ollie.primary = true;
    accounts.ollie.userName = "OliverPauffley";
    accounts.ollie.realName = "Oliver Pauffley";
    accounts.ollie.address = "mrpauffley@gmail.com";
    accounts.ollie.passwordCommand = "op get item zu2yqk3edmu7rwhacvqqac2g2e | jq -j '.details.sections[]|select(.title==\"imap\")|.fields[]|select(.k==\"concealed\")|.v'";
    accounts.ollie.flavor = "gmail.com";
    accounts.ollie.folders = {
      inbox = "INBOX";
      drafts = "drafts";
      sent = "sent";
      trash = "trash";
    };
    accounts.ollie.maildir.path = "mailbox";
  };



  accounts.email.accounts.ollie.mbsync = {
    enable = true;
    create = "both";
    expunge = "both";
    extraConfig.account = {
      # Channel Config
      remove = "both";
      sync = "all";
      syncState = "*";

      # Mailbox
      IMAPAccount gmail
      Host imap.gmail.com
      User mrpauffley@gmail.com
      PassCmd "op get item zu2yqk3edmu7rwhacvqqac2g2e | jq -j '.details.sections[]| select(.title==\"imap\") | .fields[] | select(.k==\"concealed\") | .v'"
      SSLType IMAPS
      SSLVersions TLSv1.2

      # Remote Storage
      IMAPStore gmail-remote
      Account gmail

      # Local Storage
      MaildirStore gmail-local
      Path ~/Mail/mailbox/
      Inbox ~/Mail/mailbox/INBOX

      # Channels

      Channel mailbox-local
      Master :gmail-remote:
      Slave :gmail-local:INBOX

      Channel mailbox-sent
      Master :gmail-remote:"[Gmail]/Sent Mail"
      Slave :gmail-local:sent

      Channel mailbox-drafts
      Master :gmail-remote:"[Gmail]/Drafts"
      Slave :gmail-local:drafts

      Channel mailbox-trash
      Master :gmail-remote:"[Gmail]/Drafts"
      Slave :gmail-local:drafts

      Channel mailbox-spam
      Master :gmail-remote:"[Gmail]/Spam"
      Slave :gmail-local:spam

      # Group Channels
      Group gmail
      Channel mailbox-local
      Channel mailbox-sent
      Channel mailbox-drafts
      Channel mailbox-trash
      Channel mailbox-spam
      '';

  };
  programs.msmtp = {
    enable = true;
    extraConfig = ''
      account mailbox
      host smtp.gmail.com
      port 587
      auth on
      user mrpauffley@gmail.com
      from mrpauffley@gmail.com
      passwordeval "op get item zu2yqk3edmu7rwhacvqqac2g2e | jq -j '.details.sections[]| select(.title=="imap") | .fields[] | select(.k=="concealed") | .v'"
      tls on
      tls_starttls on
      tls_trust_file /etc/ssl/certs/ca-certificates.crt
      '';
  };

  programs.neomutt.enable = true;
  accounts.email.accounts.ollie.neomutt = {
    enable = true;
    extraConfig = ''
      set my_msmtp_pass = `op get item zu2yqk3edmu7rwhacvqqac2g2e | jq - j '.details.sections [ ]| select (.title == "imap") |.fields [ ] | select (.k == "concealed") |.v'`
        # paths
        set
        folder = ~/Mail
      set header_cache = ~/.mutt/headers
      set message_cachedir = ~/.mutt/cache/bodies
      set mailcap_path = ~/.mutt/mailcap
      set tmpdir = ~/.mutt/tmp

      # basic options
      set wait_key = no
      set mbox_type = Maildir
      set timeout = 3
      set mail_check = 0
      set delete
      set quit
      set thorough_search
      set mail_check_stats
      unset confirmappend
      unset move
      unset mark_old
      unset beep_new

      # Compose view options
      set envelope_from
      set edit_headers
      set fast_reply
      set fcc_attach
      set forward_format = "Fwd: %s"
      set forward_decode
      set attribution = "On %d, %n wrote:"
      set reply_to
      set reverse_name
      set include
      set forward_quote
      set text_flowed
      unset sig_dashes
      unset mime_forward
      set editor = "nvim"

      # Status bar, date and finding stuff
      set status_chars = "*%A"
      set status_format = "[ Folder: %f ] [%r%m messages%?n? (%n new)?%?d? (%d to delete)?%?t? (%t tagged)? ]%>-%?p?( %p postponed)?"
      set date_format = "%d.%m.%Y %H:%M"
      set index_format = "[%Z] %?X?A&-? %D %-20.20F %s"
      set sort = threads
      set sort_aux = reverse-last-date-received
      set uncollapse_jump
      set sort_re
      set reply_regexp = "^(([Rr][Ee]?(\[[0-9]+\])?: *)?(\[[^]]\] *)?)*"
      set quote_regexp = "^( {0,4}[>|:#%]| {0,4}[a-z0-9]+[>|]+)+"
      set send_charset = "utf-8:iso-8859-1:us-ascii"
      set charset = "utf-8"

      # Pager view options
      set pager_index_lines = 10
      set pager_context = 3
      set pager_stop
      set menu_scroll
      set tilde
      unset markers

      # Email headers and attachments
      ignore *
      unignore from: to: cc: bcc: date: subject:
      unhdr_order *
      hdr_order from: to: cc: bcc: date: subject:
      alternative_order text/plain text/enriched text/html
      auto_view text/html

      # When composing emails use this command to get khard contacts first then everything else from mu index
      set query_command = "( khard email --parsable '%s' | sed -n '1!p'; mu cfind --format=mutt-ab '%s' )"

      # Sidebar config
      set sidebar_visible
      set sidebar_short_path
      set sidebar_folder_indent
      set sidebar_width = 25
      set sidebar_divider_char = ' | '
      set sidebar_indent_string = " /"
      set sidebar_format = "%B %* [%?N?%N / ?%S]"

      # Mailboxes to show in sidebar
      mailboxes =mailbox/INBOX
      mailboxes =mailbox/sent =mailbox/drafts =mailbox/spam =mailbox/trash

      # Sidebar navigation
      bind index,pager <down> sidebar-next
      bind index,pager <up> sidebar-prev
      bind index,pager <right> sidebar-open

      # Vim keys
      bind index,pager k previous-entry
      bind index,pager j next-entry
      bind index,pager \Cu half-up
      bind index,pager \Cd half-down
      bind index,pager g noop
      bind pager gg top
      bind index gg first-entry
      bind pager G bottom
      bind index G last-entry

      # Global shortcuts
      bind index,pager @ compose-to-sender
      bind index,pager R group-reply
      bind index,pager D purge-message
      bind index <tab> sync-mailbox
      bind index <space> collapse-thread

      # Sync all email
      macro index,pager O "<shell-escape>mbsync -a <enter>" "run mbsync to sync all mail"

      # colour options
      '';
  };
}

