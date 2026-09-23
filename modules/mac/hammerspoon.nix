{
  flake.modules.homeManager.mac = { pkgs, inputs, ... }: {
    home.file.".config/hammerspoon" = {
      source = ./hammerspoon;
      recursive = true;
    };
    home.file.".config/hammerspoon/Spoons/PaperWM.spoon" = {
      source = inputs.paperWM;
      recursive = true;
    };
    home.file.".config/hammerspoon/Spoons/WarpMouse.spoon" = {
      source = inputs.warpMouse;
      recursive = true;
    };
  };
}
