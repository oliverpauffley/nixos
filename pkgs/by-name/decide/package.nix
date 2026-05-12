{ emacsPackages, fetchFromGitHub, }:
emacsPackages.trivialBuild rec {
  pname = "decide";
  version = "latest";
  src = fetchFromGitHub {
    owner = "lifelike";
    repo = "decide-mode";
    rev = "fa97462f9c9237551e99ec56dbfe13af14391ca6";
    hash = "sha256-btL0+mpP3byfKx6GDAXDYJuqqnSZjR6W4HGh8GBgL0U=";
  };
}
