{
  flake.modules.nixos.base = { pkgs, ... }: {
    environment.systemPackages = with pkgs; [ neovim ];
    programs.vim.enable = true;
    programs.vim.defaultEditor = true;
  };
  flake.modules.homeManager.base = {
    programs.vim = {
      enable = true;
      defaultEditor = true;
    };
  };
}
