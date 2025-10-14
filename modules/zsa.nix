{
  flake.modules.nixos.base = {
    # Enable zsa keyboards
    hardware.keyboard.zsa.enable = true;
  };

  flake.modules.homeManager.base = { pkgs, ... }: {
    home.packages = [ pkgs.wally-cli pkgs.keymapp ];
  };
}
