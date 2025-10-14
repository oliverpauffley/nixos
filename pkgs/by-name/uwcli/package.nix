{ buildGoModule, fetchFromGitHub, lib, }:
buildGoModule rec {
  pname = "uw-cli";
  version = "v0.1.4";
  doCheck = false;

  src = fetchFromGitHub {
    owner = "utilitywarehouse";
    repo = pname;
    rev = "${version}";
    sha256 = "";
    private = true;
  };
  vendorHash = "";

  meta = with lib; {
    homepage = "https://github.com/${src.owner}/${pname}";
    platforms = platforms.all;
  };
}
