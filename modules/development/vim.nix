{
  flake.modules.nixos.base = { pkgs, ... }: {
    environment.systemPackages = with pkgs; [ vim neovim ];
  };
  flake.modules.homeManage.base = {
    programs.vim = {
      enable = true;
      defaultEditor = true;
    };
  };
}
