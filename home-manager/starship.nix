{ config, lib, pkgs, nix-colors, ... }: {
  programs.starship = let inherit (config.colorscheme) colors;
  in {
    enable = true;
    # Configuration written to ~/.config/starship.toml
    settings = {
      # add_newline = false;

      character = {
        success_symbol = "[~>](bold green)";
        error_symbol = "[~>](bold red)";
      };

      # package.disabled = true;
      golang = { disabled = true; };
      nix_shell = { format = "[<$name> ](bold fg:#${colors.base09})"; };
    };
  };
}
