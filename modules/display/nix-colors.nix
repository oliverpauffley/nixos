{
  flake.modules.homeManager.base = { inputs, ... }: {
    imports = [ inputs.nix-colors.homeManagerModules.default ];
    colorScheme = inputs.nix-colors.colorSchemes.dracula;
  };
}
