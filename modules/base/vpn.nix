{
  flake.modules.nixos.base = { pkgs, ... }: {
    services.mullvad-vpn.enable = true;
    services.mullvad-vpn.package = pkgs.mullvad-vpn;
    system.activationScripts.noMullvadLockdown = {
      supportsDryActivation = true;
      text = ''
        if [ "$NIXOS_ACTION" = 'dry-activate' ]; then
          echo "Dry run: mullvad lockdown-mode off"
        else
          ${pkgs.mullvad}/bin/mullvad lockdown-mode set off
        fi
      '';
    };
  };
}
