{
  flake.modules.homeManager.dev = { pkgs, ... }: {
    home.packages = with pkgs; [
      # python
      python3
      black
      pyright
      python311Packages.pyflakes
      pipenv
      python311Packages.pytest
    ];
  };
}
