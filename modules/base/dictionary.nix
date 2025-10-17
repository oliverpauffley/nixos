{
  flake.modules.nixos.base = { pkgs, ... }: {
    # enable dictionary{
    environment.etc."dict.conf".text = "server dict.org";
    environment.systemPackages = with pkgs; [ dict ];
  };
  flake.modules.homeManager.base = { pkgs, ... }: {
    home.packages = with pkgs; [
      ispell
      (aspellWithDicts (dicts: with dicts; [ en en-computers en-science ]))
      wordnet
    ];
  };
}
