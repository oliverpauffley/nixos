{ config, lib, pkgs, ... }:
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

      config.color_scheme = 'Gruvbox Dark'
      config.dpi = 180
      config.font = wezterm.font 'mononoki Nerd Font'
      config.font_size = 20.0
      config.hide_tab_bar_if_only_one_tab = true

      -- and finally, return the configuration to wezterm
      return config
    '';
  };
}
