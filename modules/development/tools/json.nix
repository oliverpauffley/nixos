{
  flake.modules.homeManager.dev = { pkgs, ... }: {
    home.packages = with pkgs; [ evans grpcurl postman ];
  };
}
