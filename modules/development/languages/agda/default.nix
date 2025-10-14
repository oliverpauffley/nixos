{
  flake.modules.homeManager.dev = { pkgs, ... }: {
    home.packages = with pkgs;
      [ (agda.withPackages [ agdaPackages.standard-library ]) ];
  };
}
