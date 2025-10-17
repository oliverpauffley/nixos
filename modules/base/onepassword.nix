{
  flake.modules.nixos.base = {
    programs._1password-gui.enable = true;
    programs._1password-gui.polkitPolicyOwners = [ "ollie" ];
    programs._1password.enable = true;
  };

}
