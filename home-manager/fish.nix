{ config, lib, pkgs, ... }: {
  programs.fish = {
    enable = true;
    shellAbbrs = {
      hms = "home-manager switch --flake .#";
      nos = "sudo nixos-rebuild switch --flake .#";
    };
    shellAliases = {
      ls = "exa";
      cat = "bat";
      giff-product-id = "uuidgen -n 01d6ade7-f2eb-5e7d-b36d-9468f7bae3fb -s -N";
    };
    interactiveShellInit = ''

      # Disable greeting
      set fish_greeting

      # Work go export
      set -Ux GOPRIVATE "github.com/utilitywarehouse/*"

      # Cargo use git to fetch
      set -Ux CARGO_NET_GIT_FETCH_WITH_CLI true

      # direnv nix shell setup
      function fish_prompt
          set -l nix_shell_info (
              if test -n "$IN_NIX_SHELL"
                  echo -n "<nix-shell>"
              end
          )

          set_color $fish_color_cwd
          echo -n (prompt_pwd)
          set_color normal
          echo -n -s "$nix_shell_info ~> "
      end

      # suppress direnv logging
      set -gx DIRENV_LOG_FORMAT ""
    '';
    functions = { ec = { body = "emacsclient --create-frame $argv &"; }; };
    plugins = [
      # Enable a plugin (here grc for colorized command output) from nixpkgs
      #{ name = "grc"; src = pkgs.fishPlugins.grc.src; }
    ];
  };
  services.gpg-agent.enableFishIntegration = true;
}
