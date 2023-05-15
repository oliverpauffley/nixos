{ inputs, config, lib, pkgs, ... }: {

  home.packages = with pkgs; [ inputs.nixpkgs-wayland.packages.${system}.swww ];

}
