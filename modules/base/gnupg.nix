{
  flake.modules.nixos.base = { pkgs, ... }: {
    programs.gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };

    security.pam.services.ollie.gnupg.enable = true;

    environment.systemPackages = with pkgs; [ gnupg ];
  };
}
