{ config, lib, pkgs, inputs, ... }:
with lib;
with types;
{
  options = {
    colours = mkOption {
      type = attrs;
      description = "my configuration colour settings";
    };
    myfonts = mkOption {
      type = attrs;
      description = "my configuration font settings";
    };
  };
  config = {
    colours = {
      bg = "#091012";
      bg_unfocused = "#353638";
      bg_critical = "#0F7A79";
      text_primary = "#FFFFFF";
      text_unfocused = "#406A99";
      text_critical = "#091012";
    };
    myfonts = {
      font-0 = "monofur for Powerline";
      font-1 = "mononoki";
    };
  };
}

