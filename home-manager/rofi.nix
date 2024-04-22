{ config, lib, pkgs, nix-colors, ... }:
let inherit (config.colorscheme) colors;
in {
  programs.rofi = {
    enable = true;
    font = config.fontProfiles.monospace.family + " 14";
    terminal = "\${pkgs.alacritty}/bin/alacritty";
    theme = let
      # Use `mkLiteral` for string-like values that should show without
      # quotes, e.g.:
      # {
      #   foo = "abc"; => foo: "abc";
      #   bar = mkLiteral "abc"; => bar: abc;
      # };
      inherit (config.lib.formats.rasi) mkLiteral;
      quote = x: ''"${x}"'';
      border-width = 5.0;
    in {
      "*" = {
        color-enabled = true;
        color-window = mkLiteral "#${colors.base02}";
        color-separator = mkLiteral "#${colors.base05}";
        color-background = mkLiteral "#${colors.base01}";
        background-color = mkLiteral "#${colors.base04}";
        blink = true;
        border-color = mkLiteral "#${colors.base0F}";
        border-radius = mkLiteral "#${colors.base03}";
        cursor = "inherit";
        placeholder = quote "Search Applications";
        placeholder-color = "#${colors.base04}";
        text-color = "#${colors.base05}";
        transparency = quote "real";
      };

      element = {
        background-color = "inherit";
        children = [ "element-icon" "element-text" ];
        cursor = "pointer";
        orientation = "vertical";
        padding = "20 0";
      };
      element-icon = { size = 64; };
      element-text = { horizontal-align = mkLiteral "0.5"; };
      "element.selected" = { border = builtins.ceil (border-width / 2.0); };
      entry = { cursor = "text"; };
      inputbar = {
        border = builtins.ceil (border-width / 2.0);
        children = [ "prompt" "entry" ];
        padding = 10;
        spacing = 10;
      };
      listview = {
        background-color = "inherit";
        children = [ "element" "scrollbar" ];
        columns = 4;
        cycle = false;
        dynamic = true;
        flow = "horizontal";
        layout = "vertical";
        lines = 2;
        scrollbar = true;
      };
      mainbox = {
        children = [ "inputbar" "listview" ];
        padding = 10;
        spacing = 10;
      };
      prompt = { enabled = true; };
      scrollbar = {
        handle-color = "#${colors.base05}";
        handle-with = 10;
      };
      window = {
        background-color = "#${colors.base04}";
        border = mkLiteral "5.0";
        width = "35%";
      };
    };
  };
}
