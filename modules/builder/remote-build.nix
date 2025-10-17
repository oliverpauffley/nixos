{ config, ... }: {
  flake.modules.nixos.use-external = {
    config = {
      nix = {
        distributedBuilds = true;
        buildMachines = [{
          hostName = "caladan";
          systems = [ "x86_64-linux" "aarch64-linux" ];
          maxJobs = 4;
          speedFactor = 2;
          supportedFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
        }];
        settings.builders-use-substitutes = true;
      };
    };
  };
  # flake.modules.nixos.remote-build = {
  #   nix.config.trusted-users =
  #     [ config.flake.meta.users.ollie.username "root" ];
  # };
}
