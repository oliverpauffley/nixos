{ emacsPackages, fetchFromGitHub, }:
emacsPackages.trivialBuild rec {
  pname = "emacs-claude-code";
  version = "latest";
  src = fetchFromGitHub {
    owner = "stevemolitor";
    repo = "claude-code.el";
    rev = "4a9914bd4161eb43f489820f9174c62390e5adc8";
    hash = "sha256-ISlD6q1hceckry1Jd19BX1MfobHJxng5ulX2gq9f644=";
  };
  # elisp dependencies
  propagatedUserEnvPkgs = with emacsPackages; [ eat transient inheritenv ];
  buildInputs = propagatedUserEnvPkgs;
}
