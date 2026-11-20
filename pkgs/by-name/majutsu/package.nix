{
  fetchFromGitHub,
  emacsPackages,
  ...
}:
emacsPackages.trivialBuild rec {
  pname = "majutsu";
  version = "unstable";

  src = fetchFromGitHub {
    owner = "0WD0";
    repo = pname;
    rev = "154ecea1812f71cf85ede346642297a22a5e1713";
    hash = "sha256-xaDI/RQg2WhndOl7OSVNlR1YbbT1IR5XaLYV+ZgbHP4=";
  };

  packageRequires = with emacsPackages; [
    magit
    transient
    with-editor
    plz
    consult
  ];
}
