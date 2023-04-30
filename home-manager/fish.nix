{ config, lib, pkgs, ... }: {
  programs.fish = {
    enable = true;
    shellAbbrs = {
      hms = "home-manager switch --flake .#";
      nos = "sudo nixos-rebuild switch --flake .#";
    };
    shellAliases = { ls = "exa"; };
    interactiveShellInit = ''

      # Disable greeting
      set fish_greeting

      # Work go export
      set -Ux GOPRIVATE "github.com/utilitywarehouse/*"

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
