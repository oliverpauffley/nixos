{
  flake.modules.homeManager.base = { config, pkgs, ... }: {
    programs.ghostty = {
      enableFishIntegration = true;
      installVimSyntax = true;
      # nix can't build ghostty from source on darwin, so use the repackaged
      # binary: https://ghostty.org/docs/install/binary#nix-(macos-binary)
      package = if pkgs.stdenv.hostPlatform.isDarwin then pkgs.ghostty-bin else pkgs.ghostty;
      settings = {
        font-size = 14;
        font-family = config.fontProfiles.regular.family;
        copy-on-select = true;
      };
      enable = true;
    };
  };
}
