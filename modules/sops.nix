{ inputs, ... }:
let
  secretspath = builtins.toString inputs.mysecrets;
  secrets = {
    ollie_passwd = { neededForUsers = true; };
    github_token = {
      neededForUsers = true;
      path = "%r/github_token";
    };
    github_username = { neededForUsers = true; };
  };
in {
  flake.modules.nixos.base = {
    imports = [ inputs.sops-nix.nixosModules.sops ];
    config = {
      sops = {
        defaultSopsFile = "${secretspath}/secrets/secrets.yaml";
        validateSopsFiles = false;

        age = {
          keyFile = "/home/ollie/.config/sops/age/keys.txt";
          generateKey = true;
        };

        secrets = secrets;
      };
    };
  };
  flake.modules.homeManager.base = { pkgs, ... }: {
    imports = [ inputs.sops-nix.homeManagerModules.sops ];

    config = {
      home.packages = with pkgs; [ age sops ];
      sops = {
        age.keyFile = "/home/ollie/.config/sops/age/keys.txt";
        age.sshKeyPaths =
          [ "/etc/ssh/ssh_host_ed25519_key" "/home/ollie/.ssh/id_rsa" ];

        defaultSopsFile = "${secretspath}/secrets/secrets.yaml";
        validateSopsFiles = false;

        secrets = { github_token.path = "%r/github_token"; };
      };
    };
  };
}
