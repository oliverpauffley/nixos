{
  flake.modules.nixos.work = {
    networking.extraHosts = "10.91.9.5 webappint.tp.private";
  };

  flake.modules.homeManager.work = { pkgs, ... }: {
    home.packages = with pkgs; [ slack ];
    programs.fish.interactiveShellInit = ''
      # Work go export and github settings
      set -Ux GOPRIVATE "github.com/utilitywarehouse/*"
    '';
  };
}
