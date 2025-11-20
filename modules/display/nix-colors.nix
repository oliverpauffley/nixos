{
  flake.modules.homeManager.base = { inputs, ... }: {
    imports = [ inputs.nix-colors.homeManagerModules.default ];
    # colorScheme = inputs.nix-colors.colorSchemes.nord;
    colorScheme = {
      slug = "vivendi";
      name = "vivendi";
      author = "prot";
      palette = {
        base00 = "#0d0e1c";
        base01 = "#1d2235";
        base02 = "#3E2D5C";
        base03 = "#042837";
        base04 = "#ef8386";
        base05 = "#ffffff";
        base06 = "#989898";
        base07 = "#c6daff";
        base08 = "#4ae2f0";
        base09 = "#ffffff";
        base0A = "#d2b580";
        base0B = "#2fafff";
        base0C = "#11c777";
        base0D = "#f78fe7";
        base0E = "#79a8ff";
        base0F = "#feacd0";
      };
    };
  };
}
