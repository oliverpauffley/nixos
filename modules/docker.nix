{
  flake.modules.nixos.dev = { pkgs, ... }: {
    virtualisation.docker.enable = true;
  };
}
