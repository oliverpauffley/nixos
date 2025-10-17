{
  flake.modules.nixos.base = { pkgs, ... }: {
    environment.systemPackages = with pkgs; [ openssl openssl.dev ];

    environment.variables = {
      PKG_CONFIG_PATH = "${pkgs.openssl.dev}/lib/pkgconfig";
    };
  };
}
