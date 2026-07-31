{ config, ... }: {
  flake.modules.darwin."hosts/kaitain" = {
    imports = with config.flake.modules.darwin; [
      base

      # Users
      ollie
    ]
    # Specific Home-Manager modules
    ++ [{
      home-manager.users.ollie.imports =
        with config.flake.modules.homeManager; [ darwin ];
    }];
  };
}
