{
  flake.modules.homeManager.base = { config, pkgs, ... }: {
    programs.ghostty = {
      installVimSyntax = true;
      # nix can't build ghostty from source on darwin, so use the repackaged
      # binary: https://ghostty.org/docs/install/binary#nix-(macos-binary)
      package = if pkgs.stdenv.hostPlatform.isDarwin then pkgs.ghostty-bin else pkgs.ghostty;
      settings = {
        font-size = 14;
        font-family = config.fontProfiles.regular.family;
        copy-on-select = true;
        macos-option-as-alt = true;
        keybind = [
          "alt+v=activate_key_table:vim"

          "vim/"

          # Line movement
          "vim/j=scroll_page_lines:1"
          "vim/k=scroll_page_lines:-1"

          # Page movement
          "vim/ctrl+d=scroll_page_down"
          "vim/ctrl+u=scroll_page_up"
          "vim/ctrl+f=scroll_page_down"
          "vim/ctrl+b=scroll_page_up"
          "vim/shift+j=scroll_page_down"
          "vim/shift+k=scroll_page_up"

          # Jump to top/bottom
          "vim/g>g=scroll_to_top"
          "vim/shift+g=scroll_to_bottom"

          # Search (if you want vim-style search entry)
          "vim/slash=start_search"
          "vim/n=navigate_search:next"

          # Copy mode / selection
          # Note we're missing a lot of actions here to make this more full featured.
          "vim/v=copy_to_clipboard"
          "vim/y=copy_to_clipboard"

          # Command Palette
          "vim/shift+semicolon=toggle_command_palette"

          # Exit
          "vim/escape=deactivate_key_table"
          "vim/q=deactivate_key_table"
          "vim/i=deactivate_key_table"

          # Catch unbound keys
          "vim/catch_all=ignore"
        ];
      };
      enable = true;
    };
  };
}
