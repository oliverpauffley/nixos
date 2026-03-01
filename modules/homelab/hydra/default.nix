{
  flake.modules.nixos.hydra = { pkgs, ... }: {
    environment.systemPackages = with pkgs; [ git ];
    services.hydra = {
      enable = true;
      hydraURL = "http://localhost:3000"; # externally visible URL
      notificationSender = "hydra@localhost"; # e-mail of hydra service
      # you will probably also want, otherwise *everything* will be built from scratch
      useSubstitutes = true;
      extraConfig = ''
        allow-import-from-derivation = true
      '';
    };

    networking.firewall.allowedTCPPorts = [ 3000 ];
    nix.buildMachines = [{
      hostName = "localhost";
      protocol = null;
      system = "x86_64-linux";
      supportedFeatures = [ "kvm" "nixos-test" "big-parallel" "benchmark" ];
      maxJobs = 8;
    }];
    nix.distributedBuilds = true;
    nix.settings.trusted-users = [ "hydra" "hydra-queue-runner" ];
    nix.settings.allow-import-from-derivation = true;
    nix.settings.allowed-uris =
      [ "github:" "https://github.com/" "git+ssh://github.com/" ];
  };
}
