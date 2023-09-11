{ config, lib, pkgs, ... }: {
  programs.nushell = {
    enable = true;
    shellAliases = {
      giff-account-id = "uuidgen -n 01d6ade7-f2eb-5e7d-b36d-9468f7bae3fb -s -N";
    };
    envFile = {
      text = ''
        $env.GOPRIVATE = "github.com/utilitywarehouse/*"
        $env.CARGO_NET_GIT_FETCH_WITH_CLI = true
        $env.DIRENV_LOG_FORMAT = "";
        $env.EDITOR = "emacsclient -nw"
      '';
    };
    configFile = {
      text = ''
        $env.PROMPT_INDICATOR = "~> "
        $env.PROMPT_INDICATOR_VI_INSERT = "~> "
        $env.PROMPT_INDICATOR_VI_NORMAL = "● "
        $env.PROMPT_MULTILINE_INDICATOR = "::: "

          let-env config = {
                      show_banner: false
                      edit_mode: vi
        }
      '';
    };
  };
}