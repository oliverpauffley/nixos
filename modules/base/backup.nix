{ lib, config, ... }:
{
  flake.modules.nixos.backup =
    { pkgs, ... }:
    let
      common-excludes = [
        "code"
        "go"
        "exercism"
        ".cache"
        ".config"
        ".local"
        ".mozilla"
        ".aws"
        "Downloads"
        ".hoogle"
        ".gnupg"
        ".cargo"
        ".stack"
        ".rustup"
      ];
      basicBorgJob = name: {
        encryption.mode = "none";
        environment.BORG_RSH = "ssh -o 'StrictHostKeyChecking=no' -i /home/ollie/.ssh/id_rsa";
        environment.BORG_UNKNOWN_UNENCRYPTED_REPO_ACCESS_IS_OK = "yes";
        extraCreateArgs = "--verbose --progress";
        repo = "ssh://root@192.168.0.100//mnt/media/${name}";
        inhibitsSleep = true;
        compression = "zstd,1";
        startAt = "12:00:00";
        user = "root";
      };
    in
    {
      services.borgbackup.jobs = {
        home-backup = basicBorgJob "backup" // rec {
          paths = "/home/ollie";
          exclude = map (x: paths + "/" + x) common-excludes;
        };
      };
      environment.systemPackages = with pkgs; [
        borgbackup
      ];

    };
}
