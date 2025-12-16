{ emacsPackages, fetchFromGitHub, }:
emacsPackages.trivialBuild rec {
  pname = "vaarn";
  version = "latest";
  src = fetchFromGitHub {
    owner = "oliverpauffley";
    repo = "vaarn.el";
    rev = "91c7a45daba11fa2c937100affd3a3906f5d95b3";
    hash = "sha256-SfiV3qPLlTdOZC9rkekw2lUxbxKKLlfalOxoDPvMgi4=";
  };
}
