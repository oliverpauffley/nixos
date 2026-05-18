{ emacsPackages, fetchFromGitHub, }:
emacsPackages.trivialBuild rec {
  pname = "structured-log-mode";
  version = "latest";
  src = fetchFromGitHub {
    owner = "lgfang";
    repo = "structured-log-mode";
    rev = "05422ed3579801f91a30534993f3fab178b78a4b";
    hash = "sha256-RHNgwTrQTNLXRSlI8mL4Vm/AafN2hF3UpRRhnIy26sI=";
  };
}
