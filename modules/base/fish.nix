{
  flake.modules.nixos.base = { pkgs, ... }: {
    environment.shells = [ pkgs.fish ];
    programs.fish.enable = true;
  };

  flake.modules.homeManager.base = { pkgs, config, inputs, ... }: {
    home.shell.enableFishIntegration = true;

    programs = {
      fish = {
        enable = true;
        plugins = [{
          name = "autopair";
          src = pkgs.fishPlugins.autopair;
        }];
        shellAliases = {
          ".." = "cd ..";
          "..." = "cd ../..";
          "cd" = "z";
          cat = "bat";
          ls = "eza";
          grep = "rg";
          icat = "kitten icat";
        };
        functions = {
          fish_greeting = "";

          rds_connect = {
            body = ''
              set -l host $argv[1]
              set -l user $argv[2]
              set -l dbname $argv[3]

              # Generate the auth token using command substitution
              set -l pass (aws rds generate-db-auth-token --hostname $host --port 5432 --region eu-west-1 --username $user)

              # Download the SSL certificate bundle
              curl https://truststore.pki.rds.amazonaws.com/eu-west-1/eu-west-1-bundle.pem > eu-west-1-bundle.pem

              # Connect using psql with the generated token
              psql "user=$user port=5432 host=$host dbname=$dbname sslmode=verify-full sslrootcert=eu-west-1-bundle.pem password=$pass"'';
          };
        };
        interactiveShellInit =
          let nix-colors-lib = inputs.nix-colors.lib.contrib { inherit pkgs; };
          in ''
            sh ${
              nix-colors-lib.shellThemeFromScheme {
                scheme = config.colorScheme;
              }
            }
            # Disable greeting
            set fish_greeting


            # Cargo use git to fetch
            set -Ux CARGO_NET_GIT_FETCH_WITH_CLI true

            # suppress direnv logging
            set -gx DIRENV_LOG_FORMAT ""

            set GITHUB_TOKEN (cat $XDG_RUNTIME_DIR/github_token | string collect)

            set -U fish_user_paths $fish_user_paths $HOME/.config/emacs/bin
          '';
      };
    };
    services.gpg-agent.enableFishIntegration = true;
    programs.zoxide = {
      enable = true;
      enableFishIntegration = true;
    };
  };
}
