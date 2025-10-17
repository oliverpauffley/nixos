{ config, inputs, ... }: {
  flake.modules.nixos."hosts/caladan" = {
    imports = with config.flake.modules.nixos;
      [
        inputs.sops-nix.nixosModules.sops
        # Modules
        base
        media
        coredns
        traefik

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
          ];
      }];
  };
}
