{ inputs, ... }:
let secretspath = builtins.toString inputs.mysecrets;
in {
  imports = [ inputs.sops-nix.homeManagerModules.sops ];

  sops = {
    age.keyFile = "/home/ollie/.config/sops/age/keys.txt";
    age.sshKeyPaths =
      [ "/etc/ssh/ssh_host_ed25519_key" "/home/ollie/.ssh/id_rsa" ];

    defaultSopsFile = "${secretspath}/secrets/secrets.yaml";
    validateSopsFiles = false;

    secrets = {
      # "private_keys/ollie" = { path = "/home/ollie/.ssh/id_ed25519"; };

      github_token = { path = "%r/github_token"; };
    };
  };
}
