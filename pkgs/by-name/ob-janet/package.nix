{ emacsPackages, fetchFromGitHub, }:
emacsPackages.trivialBuild rec {
  pname = "ob-janet";
  version = "latest";
  src = fetchFromGitHub {
    owner = "DEADB17";
    repo = "ob-janet";
    rev = "";
    hash = "";
  };
}
