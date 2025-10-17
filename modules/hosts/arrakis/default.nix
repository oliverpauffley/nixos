{ config, inputs, ... }: {
  flake.modules.nixos."hosts/arrakis" = {
    imports = with config.flake.modules.nixos;
      [
        inputs.sops-nix.nixosModules.sops
        # Modules
        base
        laptop
        work
        dev
        xmonad
        use-external

        # Users
        ollie
        root
        inputs.hardware.nixosModules.lenovo-thinkpad-x1-nano-gen1

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
