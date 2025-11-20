{
  flake.modules.homeManager.dev = { pkgs, ... }: {
    home.packages = with pkgs; [
      libtool
      gnumake
      cmake
      shellcheck
      coreutils
      cmake
      clang-tools
    ];
  };
}
