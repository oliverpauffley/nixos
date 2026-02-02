{ emacsPackages, fetchFromGitHub, }:
emacsPackages.trivialBuild rec {
  pname = "linear-emacs";
  version = "latest";
  src = fetchFromGitHub {
    owner = "anegg0";
    repo = pname;
    rev = "70baf3871d3f82a26aecf20bf366039a112d3d3f";
    hash = "sha256-CGw/8T4V1m3LUEdYkorfeI5PEo8U+vbhtug4Zvf3FfI=";
  };
  packageRequires = with emacsPackages; [ request dash s cl-lib ];
}
