{ buildGoModule, fetchFromGitHub, lib, }:
buildGoModule rec {
  pname = "multi-gitter";
  version = "0.53.0";

  src = fetchFromGitHub {
    owner = "lindell";
    repo = pname;
    rev = "v${version}";
    sha256 = "sha256-o/YY4ciHsSOcZyA6NGMzk/PdHpAnwcl3o5QG7ikSSc0=";
  };
  vendorHash = "sha256-y5/To+IQ+sKKYcif3wTGeLhbQPcONn5q6PKhSHrcYDU=";

  doCheck = false;
  # TODO shell completion
  meta = with lib; {
    description = "update multiple github repos at once";
    homepage = "https://github.com/lindell/multi-gitter";
    platforms = platforms.all;
  };
}
