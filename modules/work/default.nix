{
  flake.modules.nixos.work = { pkgs, ... }: {
    networking.extraHosts = "10.91.9.5 webappint.tp.private";
    environment.systemPackages = with pkgs.local; [ strongbox ];
  };

  flake.modules.homeManager.work = { pkgs, ... }: {
    home.packages = with pkgs; [ slack ];
    programs.fish.interactiveShellInit = ''
      # Work go export and github settings
      set -Ux GOPRIVATE "github.com/utilitywarehouse/*"
    '';
  };
}
