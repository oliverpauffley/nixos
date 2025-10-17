{
  flake.modules.nixos.media = { pkgs, inputs, ... }: {
    nixpkgs.overlays = [ inputs.self.overlays.default ];
    systemd.timers = {
      "dovi-convert-tv" = {
        timerConfig = {
          OnCalender = "*-*-* 02:00:00";
          Unit = "dovi-convert-tv.service";
        };
      };
      "dovi-convert-film" = {
        timerConfig = {
          OnCalender = "*-*-* 02:00:00";
          Unit = "dovi-convert-film.service";
        };
      };
    };
    systemd.services = {
      "dovi-convert-tv" = {
        description = "dovi convert all tv files";
        serviceConfig = {
          ExecStart =
            "${pkgs.local.dovi-convert}/bin/dovi-convert /mnt/media/tv";
        };
      };
      "dovi-convert-film" = {
        description = "dovi convert all film files";
        serviceConfig = {
          ExecStart =
            "${pkgs.local.dovi-convert}/bin/dovi-convert /mnt/media/movies";
        };
      };
    };
  };
}
