{ pkgs, config, ... }:

{
  accounts.email = {
    maildirBasePath = "/home/ollie/Mail";
    accounts.ollie.primary = true;
    accounts.ollie.userName = "OliverPauffley";
    accounts.ollie.realName = "Oliver Pauffley";
    accounts.ollie.address = "mrpauffley@gmail.com";
    accounts.ollie.passwordCommand = "pass gmail/imap";
    accounts.ollie.flavor = "gmail.com";
    accounts.ollie.folders = {
      inbox = "INBOX";
      drafts = "drafts";
      sent = "sent";
      trash = "trash";
    };
    accounts.ollie.maildir.path = "mailbox";
  };



  programs.mbsync = {
    enable = true;
    extraConfig = ''
      # Channel Config
      Create Both
      Expunge Both
      Remove Both
      Sync All
      SyncState *

      # Mailbox
      IMAPAccount gmail
      Host imap.gmail.com
      User mrpauffley@gmail.com
      PassCmd "pass gmail/imap"
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
      passwordeval "pass gmail/imap"
      tls on
      tls_starttls on
      tls_trust_file /etc/ssl/certs/ca-certificates.crt
    '';
  };

  programs.neomutt.enable = true;
  accounts.email.accounts.ollie.neomutt = {
    enable = true;
    sendMailCommand = ''${pkgs.msmtp}/bin/msmtp'';
    extraConfig = ''
                  # paths
                  set folder = ~/Mail
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
                  # gruvbox dark (contrast dark):

            # bg0    = 234
            # bg1    = 237
            # bg2    = 239
            # bg3    = 241
            # bg4    = 243
            # 
            # gray   = 245
            # 
            # fg0    = 229
            # fg1    = 223
            # fg2    = 250
            # fg3    = 248
            # fg4    = 246
            # 
            # red    = 167
            # green  = 142
            # yellow = 214
            # blue   = 109
            # purple = 175
            # aqua   = 108
            # orange = 208


            # See http://www.mutt.org/doc/manual/#color

                  color attachment  color109 color234
                  color bold        color229 color234
                  color error       color167 color234
                  color hdrdefault  color246 color234
                  color indicator   color223 color237
                  color markers     color243 color234
                  color normal      color223 color234
                  color quoted      color250 color234
                  color quoted1     color108 color234
                  color quoted2     color250 color234
                  color quoted3     color108 color234
                  color quoted4     color250 color234
                  color quoted5     color108 color234
                  color search      color234 color208
                  color signature   color108 color234
                  color status      color234 color250
                  color tilde       color243 color234
                  color tree        color142 color234
                  color underline   color223 color239

                  color sidebar_divider    color250 color234
                  color sidebar_new        color142 color234

                  color index color142 color234 ~N
                  color index color108 color234 ~O
                  color index color109 color234 ~P
                  color index color214 color234 ~F
                  color index color175 color234 ~Q
                  color index color167 color234 ~=
                  color index color234 color223 ~T
                  color index color234 color167 ~D

                  color header color214 color234 "^(To:|From:)"
                  color header color142 color234 "^Subject:"
                  color header color108 color234 "^X-Spam-Status:"
                  color header color108 color234 "^Received:"

                  color body color142 color234 "[a-z]{3,256}://[-a-zA-Z0-9@:%._\\+~#=/?&,]+"
                  #color body color142 color234 "[a-zA-Z]([-a-zA-Z0-9_]+\\.){2,256}[-a-zA-Z0-9_]{2,256}"
                  color body color208 color234 "[-a-z_0-9.%$]+@[-a-z_0-9.]+\\.[-a-z][-a-z]+"
                  color body color208 color234 "mailto:[-a-z_0-9.]+@[-a-z_0-9.]+"
                  color body color234 color214 "[;:]-*[)>(<lt;|]"
                  color body color229 color234 "\\*[- A-Za-z]+\\*"

                  color body color214 color234 "^-.*PGP.*-*"
                  color body color142 color234 "^gpg: Good signature from"
                  color body color167 color234 "^gpg: Can't.*$"
                  color body color214 color234 "^gpg: WARNING:.*$"
                  color body color167 color234 "^gpg: BAD signature from"
                  color body color167 color234 "^gpg: Note: This key has expired!"
                  color body color214 color234 "^gpg: There is no indication that the signature belongs to the owner."
                  color body color214 color234 "^gpg: can't handle these multiple signatures"
                  color body color214 color234 "^gpg: signature verification suppressed"
                  color body color214 color234 "^gpg: invalid node with packet of type"

                  color compose header            color223 color234
      color compose security_encrypt  color175 color234
      color compose security_sign     color109 color234
      color compose security_both     color142 color234
      color compose security_none     color208 color234
    '';
  };
}
