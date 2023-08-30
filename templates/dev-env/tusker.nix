{ pkgs }:

let version = "0.5.0";
in pkgs.poetry2nix.mkPoetryApplication rec {
  pname = "tusker";
  inherit version;

  src = pkgs.fetchFromGitHub {
    owner = "bikeshedder";
    repo = "tusker";
    rev = "v${version}";
    hash = "sha256-TLeajkWREPSnusuYcYldg+Uyof0h9caUY/7BZE2+4nM=";
  };

  projectDir = src;

  overrides = pkgs.poetry2nix.overrides.withDefaults (self: super:
    let
      addInputs = inputs: old: {
        propagatedBuildInputs = (old.propagatedBuildInputs or [ ]) ++ inputs;
      };
      addSetupTools = addInputs [ self.setuptools ];
    in {
      sqlbag = super.sqlbag.overridePythonAttrs addSetupTools;
      schemainspect = super.schemainspect.overridePythonAttrs addSetupTools;
      migra = super.migra.overridePythonAttrs (addInputs [ self.poetry ]);
    });
}
