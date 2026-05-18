{
  flake.modules.homeManager.dev = { pkgs, config, ... }: {
    home.packages = with pkgs; [ hl-log-viewer ];
    programs.k9s = {
      enable = true;

      settings = {
        k9s = {
          liveViewAutoRefresh = true;
          refreshRate = 4;
          maxConnRetry = 5;
          skipLatestRevCheck = true;
          noExitOnCtrlC = true;
          ui = {
            enableMouse = false;
            headless = false;
            logoless = true;
            crumbsless = true;
            reactive = true;
            noIcons = false;
          };
          logger = {
            tail = 1024;
            buffer = 8192;
            sinceSeconds = -1;
            fullScreen = false;
            textWrap = true;
            showTime = false;
          };
        };
      };

      # Community plugins: https://github.com/derailed/k9s/tree/master/plugins
      plugins = {
        emacs-logs = {
          shortCut = "Shift-E";
          confirm = false;
          description = "Search Logs in Emacs";
          scopes = [
            "pods"
            "deployments"
            "replicasets"
            "statefulsets"
            "daemonsets"
            "jobs"
            "services"
          ];
          command = "bash";
          background = false;
          args = [
            "-c"
            ''
              TMPFILE=$(mktemp /tmp/k9s-log-$NAME.XXXXXX)

              ${pkgs.kubectl}/bin/kubectl logs $NAME -n $NAMESPACE --context $CONTEXT --all-containers=true > "$TMPFILE" 2>&1

              ${pkgs.emacs}/bin/emacsclient -nw --eval \
                "(let ((buf (find-file-noselect \"$TMPFILE\")))
                   (with-current-buffer buf
                     (rename-buffer (format \"*k9s-logs: %s*\" \"$NAME\") t)
                     (read-only-mode 1)
                     (view-mode 1)
                     (local-set-key (kbd \"q\") 'kill-current-buffer))
                   (switch-to-buffer buf))"

              # 4. Cleanup
              rm -f "$TMPFILE"
            ''
          ];
        };
        # json
        jsonlogs = {
          shortCut = "Shift-L";
          confirm = false;
          command = "sh";
          description = "Pretty json logs";
          scopes = [
            "daemonset"
            "deploy"
            "job"
            "pod"
            "replicaset"
            "service"
            "statefulset"
            "log"
            "container"
          ];
          args = [
            "-c"
            ''
              ${pkgs.kubectl}/bin/kubectl logs pod/"$POD" --context="$CONTEXT" --namespace="$NAMESPACE" -c "$NAME" | ${pkgs.jq}/bin/jq -SR "try fromjson catch ." | ${pkgs.less}/bin/less
            ''
          ];

        };

        # secrets
        decode-secret = {
          shortCut = "Shift-X";
          confirm = false;
          description = "Decode Secret";
          scopes = [ "secrets" ];
          command = "bash";
          background = false;
          args = [
            "-c"
            "kubectl get secret $NAME -n $NAMESPACE -o json | jq '.data | map_values(@base64d)' | less"
          ];
        };

        # ArgoCD plugins
        argocd = {
          shortCut = "Shift-A";
          description = "Sync ArgoCD Application";
          scopes = [ "applications" ];
          command = "argocd";
          background = false;
          confirm = false;
          args = [ "app" "sync" "$NAME" "--app-namespace" "$NAMESPACE" ];
        };
        argocd-refresh-apps = {
          shortCut = "Shift-F";
          description = "Force refresh an ArgoCD application";
          scopes = [ "applications" ];
          command = "kubectl";
          background = false;
          confirm = false;
          args = [
            "--kubeconfig"
            "$KUBECONFIG"
            "--context"
            "$CONTEXT"
            "--namespace"
            "$NAMESPACE"
            "annotate"
            "applications"
            "$NAME"
            "argocd.argoproj.io/refresh=hard"
          ];
        };
        argocd-disable-auto-sync = {
          shortCut = "Shift-D";
          description = "Disable ArgoCD sync";
          scopes = [ "applications" ];
          command = "kubectl";
          background = false;
          confirm = false;
          args = [
            "--kubeconfig"
            "$KUBECONFIG"
            "--context"
            "$CONTEXT"
            "--namespace"
            "$NAMESPACE"
            "patch"
            "applications"
            "$NAME"
            "--type=json"
            ''-p=[{"op":"replace", "path": "/spec/syncPolicy", "value": {}}]''
          ];
        };
      };

      aliases = {
        dp = "deployments";
        sec = "v1/secrets";
        jo = "jobs";
        cr = "clusterroles";
        crb = "clusterrolebindings";
        ro = "roles";
        rb = "rolebindings";
        np = "networkpolicies";
      };
    };
  };
}
