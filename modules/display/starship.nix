{
  flake.modules.homeManager.base = { config, lib, pkgs, nix-colors, ... }: {
    programs.starship = let inherit (config.colorscheme) palette;
    in {
      enable = true;
      settings = {
        # TODO change font
        directory = { style = "bold blue"; };

        character = {
          success_symbol = "[~>](bold blue)";
          error_symbol = "[~>](bold red)";
        };

        palette = "base-16";

        palettes = {
          "base-16" = {
            black = "#${palette.base02}";
            red = "#${palette.base09}";
            green = "#${palette.base0B}";
            blue = "#${palette.base0D}";
            yellow = "#${palette.base06}";
            purple = "#${palette.base0C}";
            cyan = "#${palette.base0D}";
            white = "#${palette.base01}";
          };
        };

        # package.disabled = true;
        golang = { disabled = true; };
        nix_shell = { format = "[<$name> ](bold red)"; };
      };
    };
  };
}
