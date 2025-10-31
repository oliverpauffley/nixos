{ lib, inputs, ... }: {
  flake.modules = {
    nixos.base = {
      security.rtkit.enable = true;

      services = {
        pulseaudio.enable = false;
        pipewire = {
          enable = true;
          extraConfig = {
            pipewire."99-silent-bell.conf" = {
              "context.properties" = { "module.x11.bell" = false; };
            };
          };
          alsa = {
            enable = true;
            support32Bit = true;
          };
          pulse.enable = true;

        };
      };
    };

    homeManager.base = { pkgs, config, ... }: {
      home.packages = (with pkgs; [ pavucontrol qpwgraph ]);
    };
  };
}
