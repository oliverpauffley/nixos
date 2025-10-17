{ config, inputs, ... }: {
  flake.modules.nixos."hosts/giedi-prime" = {
    imports = with config.flake.modules.nixos;
      [
        inputs.sops-nix.nixosModules.sops
        # Modules
        base
        dev
        xmonad
        game

        # Users
        ollie
        root
      ]
      # Specific Home-Manager modules
      ++ [{
        home-manager.users.ollie.imports =
          with config.flake.modules.homeManager; [
            inputs.sops-nix.homeManagerModules.sops
            base
            work
            dev
            xmonad
          ];
      }];
  };
}
