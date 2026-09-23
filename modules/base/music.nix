{
  flake.modules.homeManager.music = {pkgs, inputs, ...}: {
    home.packages = with pkgs;[
      local.vcv-rack
    ];
  };
}
