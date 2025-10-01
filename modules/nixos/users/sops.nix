{ inputs, config, ... }:
let secretspath = builtins.toString inputs.mysecrets;
in {
  imports = [ inputs.sops-nix.nixosModules.sops ];

  sops = {
    defaultSopsFile = "${secretspath}/secrets/secrets.yaml";
    validateSopsFiles = false;

    age = {
      keyFile = "/home/ollie/.config/sops/age/keys.txt";
      # sshKeyPaths = [ "/home/ollie/.ssh/id_rsa" ];
      generateKey = true;
    };

    secrets = {
      ollie_passwd = { neededForUsers = true; };
      github_token = { neededForUsers = true; };
      github_username = { neededForUsers = true; };
    };
  };
}
