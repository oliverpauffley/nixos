{
  flake.modules.homeManager.dev = {pkgs, inputs, ...}: {
    janet
    jpm
  };
}
