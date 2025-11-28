{ emacsPackages, fetchFromGitHub, }:
emacsPackages.trivialBuild rec {
  pname = "meow-tree-sitter";
  version = "latest";
  src = fetchFromGitHub {
    owner = "skissue";
    repo = "meow-tree-sitter";
    rev = "ef69efbbc5a7d13c29ce871327a51622e03eafc7";
    hash = "sha256-SjAdwP5W8iToI8nCg9WAnhfEodNZx/Rs7mF+N4hTK34";
  };
  # elisp dependencies
  propagatedUserEnvPkgs = [ emacsPackages.meow ];
  buildInputs = propagatedUserEnvPkgs;
}
