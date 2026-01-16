{ emacsPackages, fetchFromGitHub, }:
emacsPackages.trivialBuild rec {
  pname = "ob-janet";
  version = "latest";
  src = fetchFromGitHub {
    owner = "DEADB17";
    repo = "ob-janet";
    rev = "b138d1739c6eb33d4c517ee8410afc3a390d8832";
    hash = "sha256-nzuG6FFMdUXIKvUt6McxtskvuWmsyNqDBep51TufT7o=";
  };
}
