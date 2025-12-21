{ config, ... }: {
  flake = {
    meta.users = {
      ollie = {
        email = "mrpauffley@gmail.com";
        name = "Oliver Pauffley";
        username = "ollie";
        key = "898E9AF3BA558BBD27CCEC76776333D265A54BED";
        authorizedKeys = [
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEBi5jJc061Z7WuNVEgqDH3Ws9FNr9scABwzkkRHZWPl"
        ];
      };
    };

    modules.nixos.ollie = local@{ pkgs, ... }: {
      programs.fish.enable = true;
      users.users.ollie = {
        description = config.flake.meta.users.ollie.name;
        isNormalUser = true;
        createHome = true;
        extraGroups =
          [ "audio" "input" "networkmanager" "sound" "tty" "wheel" "docker" ];
        shell = pkgs.fish;
        openssh.authorizedKeys.keys =
          config.flake.meta.users.ollie.authorizedKeys;
        hashedPasswordFile = local.config.sops.secrets.ollie_passwd.path;
      };

      nix.settings.trusted-users =
        [ config.flake.meta.users.ollie.username "root" ];


      home-manager.users.ollie = {
        home.file = {
          ".face" = {
            source = ../../../files/home/ollie/.face;
            recursive = true;
          };
          ".face.icon" = {
            source = ../../../files/home/ollie/.face;
            recursive = true;
          };
          # Credits to https://store.kde.org/p/1272202
          "Pictures/Backgrounds/" = {
            source = ../../../files/home/ollie/Pictures/Backgrounds;
            recursive = true;
          };
        };
      };
    };
  };
}
