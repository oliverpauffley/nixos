{
  flake.modules.homeManager.dev = { pkgs, inputs, ... }: {
    nixpkgs.overlays = [ inputs.self.overlays.default ];
    home.packages = with pkgs; [
      xan # process csvs from shell
      imagemagick
      gifsicle
      pkg-config
      eclint
      just
      mermaid-cli
      local.multi-gitter
      exercism
      claude-code
      claude-agent-acp
      postgresql.pg_config

    ];
  };
}
