{
  flake.modules.nixos.base = { pkgs, ... }: {

    environment.shells = [ pkgs.fish ];
    programs.fish.enable = true;
  };

  flake.modules.homeManager.base = { pkgs, ... }: {
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
          cat = "bat";
          ls = "eza";
          grep = "rg";
          icat = "kitten icat";
        };
        functions = { fish_greeting = ""; };

        interactiveShellInit = ''
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
