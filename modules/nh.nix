{
  flake.modules.nixos.base = {
    programs = {
      nh = {
        enable = true;
        clean = {
          enable = true;
          extraArgs = "--keep 2";
        };
      };
    };
  };
}
