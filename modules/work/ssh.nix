{
  flake.modules.darwin.work = { pkgs, ... }: {
    programs.ssh = {
      extraConfig = ''
        Host brossa.io
            User root
            IdentityFile /Users/ollie/.ssh/id_brossa_cd
            IdentitiesOnly Yes
      '';
    };

  };
}
