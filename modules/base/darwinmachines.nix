{ inputs, lib, config, ... }:
let prefix = "hosts/";
in {
  flake.darwinConfigurations = lib.pipe config.flake.modules.darwin [
    (lib.filterAttrs (name: _: lib.hasPrefix prefix name))
    (lib.mapAttrs' (name: module:
      let
        specialArgs = {
          inherit inputs;
          hostConfig = { name = lib.removePrefix prefix name; };
        };
      in {
        name = lib.removePrefix prefix name;
        value = inputs.nix-darwin.lib.darwinSystem {
          inherit specialArgs;
          modules = [
            module
            inputs.home-manager.darwinModules.home-manager
            {
              home-manager.extraSpecialArgs = specialArgs;
              home-manager.backupFileExtension = "bck";

            }
          ];
        };
      }))
  ];
}
