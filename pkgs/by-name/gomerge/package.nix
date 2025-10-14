{ buildGoModule, fetchFromGitHub, lib, }:
buildGoModule rec {
  pname = "gomerge";
  version = "3.4.0";

  src = fetchFromGitHub {
    owner = "Cian911";
    repo = pname;
    rev = "${version}";
    sha256 = "sha256-P+v7nvMWly6kB6QtBZr3RfbkOPQ5ZjPl7bBiUNjYCjM=";
  };
  vendorHash = "sha256-ToUz4vRWLKttvhEnAD6B/H+1tA4lJeEgga/SSXAc2lo=";

  meta = with lib; {
    description = "go merge tool";
    homepage = "https://github.com/${src.owner}/${pname}";
    platforms = platforms.all;
  };
}
