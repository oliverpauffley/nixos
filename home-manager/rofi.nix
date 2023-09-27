{
  config,
  lib,
  pkgs,
  nix-colors,
  ...
}: let
  colors = config.colorscheme.colors;
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
    in {
      "*" = {
        color-enabled = true;
        color-window = mkLiteral "#${colors.base01}";
        color-separator = mkLiteral "#${colors.base05}";
        color-background = mkLiteral "#${colors.base01}";

        # ! Property Name     BG       FG       BG-alt   Head-BG  Head-FG
        # rofi.color-normal:  #3a3432, #a5a2a2, #3a3432, #3a3432, #f7f7f7
        # rofi.color-active:  #3a3432, #01a0e4, #3a3432, #3a3432, #01a0e4
        # rofi.color-urgent:  #3a3432, #db2d20, #3a3432, #3a3432, #db2d20

        # ! Set the desired separator style
        # rofi.separator-style: solid
      };
    };
  };
}
