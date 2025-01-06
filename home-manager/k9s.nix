{ config, lib, pkgs, ... }:
let inherit (config.colorscheme) palette;
in {
  programs.k9s = {
    package = pkgs.unstable.k9s;
    enable = true;
    # TODO add this by updating homemanager
    # plugin = {
    #   # Sends logs over to jq for processing. This leverages kubectl plugin kubectl-jq.
    #   jqlogs = {
    #     shortCut = "Ctrl-J";
    #     confirm = false;
    #     description = "Logs (jq)";
    #     scopes = [ "po" ];
    #     command = "kubectl";
    #     background = false;
    #     args = [ "jq" "$NAME" "$NAMESPACE" "$CONTEXT" ];
    #   };
    # };
    settings = {
      k9s = {
        skins.skin = "base16";
      };
    };
    skins = let
      foreground = "#${palette.base05}";
      background = "#${palette.base02}";
      current_line = "#8c6c3e";
      selection = "#364a82";
      comment = "#565f89";
      cyan = "#7dcfff";
      green = "#9ece6a";
      yellow = "#e0af68";
      orange = "#ff9e64";
      magenta = "#bb9af7";
      blue = "#7aa2f7";
      red = "#f7768e";
      purple = "#9d7cd8";
      pink = "#bb9af7";
      white = "#a9b1d6";
      black = "#1d202f";
    in {
      base16 = {
        k9s = {
          # General K9s styles
          body = {
            fgColor = "${foreground}";
            logoColor = "${blue}";
          };
          # Command prompt styles
          prompt = {
            fgColor = "${foreground}";
            bgColor = "${background}";
            suggestColor = "${orange}";
          };
          # ClusterInfoView styles.
          info = {
            fgColor = "${magenta}";
            sectionColor = "${blue}";
          };
          # Dialog styles.
          dialog = {
            fgColor = "${foreground}";
            buttonFgColor = "${foreground}";
            buttonBgColor = "${magenta}";
            buttonFocusFgColor = "${background}";
            buttonFocusBgColor = "${foreground}";
            labelFgColor = "${comment}";
            fieldFgColor = "${foreground}";
          };
          frame = {
            # Borders styles.
            border = {
              fgColor = "${selection}";
              focusColor = "${foreground}";
            };
            menu = {
              fgColor = "${foreground}";
              keyColor = "${magenta}";
              # Used for favorite namespaces
              numKeyColor = "${magenta}";
            };
            # CrumbView attributes for history navigation.
            crumbs = {
              fgColor = "${white}";
              bgColor = "${cyan}";
              activeColor = "${yellow}";
            };
            # Resource status and update styles
            status = {
              newColor = "${magenta}";
              modifyColor = "${blue}";
              addColor = "${green}";
              errorColor = "${red}";
              highlightcolor = "${orange}";
              killColor = "${comment}";
              completedColor = "${comment}";
            };
            # Border title styles.
            title = {
              fgColor = "${foreground}";
              highlightColor = "${blue}";
              counterColor = "${magenta}";
              filterColor = "${magenta}";
            };
          };
          views = {
            # Charts skins...
            charts = {
              defaultDialColors = [ "${blue}" "${red}" ];
              defaultChartColors = [ "${blue}" "${red}" ];
            };
            # TableView attributes.
            table = {
              fgColor = "${foreground}";
              cursorFgColor = "${cyan}";
              cursorBgColor = "${background}";
              markColor = "${yellow}";
            };
            # Header row styles.
            header = {
              fgColor = "${foreground}";
              sorterColor = "${cyan}";
            };
            # Xray view attributes.
            xray = {
              fgColor = "${foreground}";
              cursorColor = "${current_line}";
              graphicColor = "${blue}";
              showIcons = false;
            };
            # YAML info styles.
            yaml = {
              keyColor = "${magenta}";
              colonColor = "${blue}";
              valueColor = "${foreground}";
            };
            # Logs styles.
            logs = {
              fgColor = "${foreground}";
              indicator = {
                fgColor = "${foreground}";
                bgColor = "${selection}";
              };
            };
            help = {
              fgColor = "${foreground}";
              indicator = {
                fgColor = "${red}";
                bgColor = "${selection}";
              };
            };
          };
        };

      };
    };
  };
}
