{
  description = "Ollie's flake templates";

  inputs = {
    official-templates.url = "github:NixOS/templates";
    cachix-templates.url = "github:cachix/devenv";
  };

  outputs = { self, official-templates, cachix-templates, ... }: {
    templates = {
      dev-env = {
        path = ./dev-env;
        description = "A dev environment in a flake";
      };
    } // official-templates.templates // cachix-templates.templates;
  };
}
