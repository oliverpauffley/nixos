{
  flake.modules.homeManager.base = { pkgs, config, ... }: {
    home.packages = with pkgs; [
    ];
    programs.jujutsu = {
      enable = true;
      settings = {
        user = {
          email = "mrpauffley@gmail.com";
          name = "oliverhain";
        };
        signing = {
          behaviour = "own";
          backend = "gpg";
          key = "898E9AF3BA558BBD27CCEC76776333D265A54BED";
        };
      };
    };

    programs.mergiraf.enableJujutsuIntegration = true;
  };
}
