{ config, lib, pkgs, ... }:


{
  services.dunst = {
    enable = true;
    settings = {
      global = {
        monitor = 0;
        geometry = "0x0-30+20";
        shrink = "no";
        padding = 16;
        horizontal_padding = 16;
        frame_width = 0;
        frame_color = "#000000";
        separator_color = "frame";
        font = "${config.myfonts .font-0} 10";
        line_height = 4;
        markup = "full";
        format = ''%s\n%b'';
        alignment = "left";
        word_wrap = "yes";
        ignore_newline = "no";
        show_indicators = "no";
        startup_notification = false;
        hide_duplicate_count = true;
      };
      urgency_low = {
        background = config.colours.bg;
        foreground = config.colours.text_primary;
        frame_color = config.colours.bg_unfocused;
        timeout = 4;
      };
      urgency_normal = {
        background = config.colours.bg;
        foreground = config.colours.text_primary;
        frame_color = config.colours.bg_unfocused;
        timeout = 4;
      };
      urgency_critical = {
        background = config.colours.bg_critical;
        foreground = config.colours.text_critical;
        frame_color = config.colours.bg;
        timeout = 4;
      };
    };
  };
}

