{ config, lib, pkgs, nix-colors, ... }:
let colors = config.colorScheme.colors;
in
{
  programs.wezterm = {
    enable = true;
    extraConfig = ''
        local wezterm = require 'wezterm'

        -- This table will hold the configuration.
        local config = {}

        -- In newer versions of wezterm, use the config_builder which will
        -- help provide clearer error messages
        if wezterm.config_builder then
          config = wezterm.config_builder()
        end

      config.colors = {
        -- The default text color
        foreground = '#${colors.base05}',
        -- The default background color
        background = '#${colors.base00}',

        -- Overrides the cell background color when the current cell is occupied by the
        -- cursor and the cursor style is set to Block
        cursor_bg = '#${colors.base02}',
        -- Overrides the text color when the current cell is occupied by the cursor
        cursor_fg = '#${colors.base0B}',
        -- Specifies the border color of the cursor when the cursor style is set to Block,
        -- or the color of the vertical or horizontal bar when the cursor style is set to
        -- Bar or Underline.
        cursor_border = '#${colors.base02}',

        -- the foreground color of selected text
        selection_fg = '#${colors.base08}',
        -- the background color of selected text
        selection_bg = '#${colors.base02}',

        -- The color of the scrollbar "thumb"; the portion that represents the current viewport
        scrollbar_thumb = '#${colors.base0A}',

        -- The color of the split lines between panes
        split = '#${colors.base04}',
        }

        config.dpi = 180
        config.font = wezterm.font 'mononoki Nerd Font'
        config.font_size = 20.0
        config.hide_tab_bar_if_only_one_tab = true

        -- and finally, return the configuration to wezterm
        return config
    '';
  };
}
