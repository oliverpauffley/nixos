{ config, inputs, ... }: {
  flake.modules.nixos."hosts/arrakis" = {
    imports =
      with config.flake.modules.nixos;
      [
        inputs.sops-nix.nixosModules.sops
        # Modules
        base
        laptop
        work
        dev
        niri
        linux
        backup
        dev

        # Users
        ollie
        root

      ]
      # Specific Home-Manager modules
      ++ [
        {
          home-manager.users.ollie.imports = with config.flake.modules.homeManager; [
            inputs.sops-nix.homeManagerModules.sops
            base
            linux
            work
            dev
            niri
            noctalia
            dev

          ];
        }
      ];
  };
}
