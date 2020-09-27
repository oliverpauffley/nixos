{ pkgs,config, ... }:

let
  bg = config.colours.bg;
  fg = config.colours.text_primary;

  # Colored
  primary = config.colours.text_primary;

  # Dark
  secondary = config.colours.bg_unfocused;

  # Colored (light)
  tertiary =  config.colours.text_unfocused;

  # white
  quaternary = config.colours.text_primary;

  # middle gray
  quinternary = config.colours.bg_unfocused;

  # Red
  urgency = config.colours.bg_critical;
in
  {
    services.polybar = { 
      enable = true;
    
      package = pkgs.polybar.override {
        i3GapsSupport = true;
        alsaSupport = true;
      };

      script = "polybar main &";
    
      config = {
          
        "bar/main" = {
          background = "${bg}";
          foreground = "${fg}";
          modules-left = "i3";
          modules-right = "battery date";
          border-left-size = 1;
          border-left-color = "${bg}";
          border-right-size = 1;
          border-right-color = "${bg}";
          border-top-size = 2;
          border-top-color = "${bg}";
          border-bottom-size = 2;
          border-bottom-color = "${bg}";
          font-0 = "${config.myfonts.font-1}:pixelsize=10;1";
          font-1 = "${config.myfonts.font-0}:pixelsize=10;1";
        };
        "module/battery" = {
          type = "internal/battery";
        };
        "module/date" = let
          calnotify = pkgs.writeShellScript "calnotify.sh" ''
            day="${pkgs.coreutils}/bin/date +'%-d ' | ${pkgs.gnused}/bin/sed 's/\b[0-9]\b/ &/g')"
            cal="${pkgs.utillinux}/bin/cal | ${pkgs.gnused}/bin/sed -e 's/^/ /g' -e 's/$/ /g' -e "s/$day/\<span color=\'${primary}\'\>\<b\><\/span\>/" -e '1d')"
            top="${pkgs.utillinux}/bin/cal | ${pkgs.gnused}/bin/sed '1!d')"

            ${pkgs.libnotify}/bin/notify-send "$top" "$cal"
            '';
        in {
          type = "internal/date";
          date = "%I:%M %p %a %b %d";
          label = "%{A1:${calnotify}:}%date%%{A}";
          format = "<label>";
          label-padding = 5;
        };
        "module/i3" = {
          type = "internal/i3";
          label-unfocused-foreground = "${tertiary}";
          label-urgent-foreground = "${urgency}";
          label-unfocused-padding = 1;
          label-focused-padding = 1;
          label-urgent-padding = 1;
        };
   };
 };
 }
