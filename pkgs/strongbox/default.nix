{ buildGoModule, fetchFromGitHub, lib }:

buildGoModule rec {
  pname = "strongbox";
  version = "1.1.0";

  src = fetchFromGitHub {
    owner = "uw-labs";
    repo = pname;
    rev = "v${version}";
    sha256 = "sha256-ovuW/la0sRrIVCyxmdmSjxlhQT3duJrlebaTIJ9IxCs=";
  };
  vendorSha256 = "sha256-huEkc3XWbRE/9IFEM4HpTaIU4GBqqBPH0/xmFVaY/5s=";

  meta = with lib; {
    description = "Encryption for git users";
    homepage = https://github.com/uw-labs/strongbox;
    platforms = platforms.all;
  };
}
