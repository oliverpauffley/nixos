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
  modSha256 = "sha256-E59VhZQQrzEgbD+ZVHvO0Dq4ytYJIjG+V+629l4B+YB=";
  vendorSha256 = "sha256-E59VhZQQrzEgbD+ZVHvO0Dq4ytYJIjG+V+629l4B+YC=";

  meta = with lib; {
    description = "Encryption for git users";
    homepage = https://github.com/uw-labs/strongbox;
    platforms = platforms.all;
  };
}
