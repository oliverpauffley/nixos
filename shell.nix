{
  perSystem = { pkgs, self', ... }: {
    devShells.default = pkgs.mkShell {
      NIX_CONFIG = ''
        experimental-features = nix-command flakes
        extra-experimental-features = pipe-operators
      '';
      nativeBuildInputs = with pkgs; [ nix home-manager git sops colmena ];
    };
  };
}
