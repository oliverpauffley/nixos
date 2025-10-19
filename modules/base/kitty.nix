{
  flake.modules.homeManager.base = { config, ... }: {
    programs.kitty = {
      shellIntegration.enableFishIntegration = true;
      themeFile = "Dracula";
      font = {
        name = config.fontProfiles.monospace.family;
        size = 14;
      };
      enable = true;
      settings = {
        enable_audio_bell = false;
        copy_on_select = true;
        allow_remote_control = true;
        editor = "vim";
        update_check_interval = 0;
      };
    };
  };
}
