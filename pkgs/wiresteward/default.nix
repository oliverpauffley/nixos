{ buildGoModule, fetchFromGitHub, lib }:

buildGoModule rec {
  pname = "wiresteward";
  version = "0.2.6";

  src = fetchFromGitHub {
    owner = "utilitywarehouse";
    repo = pname;
    rev = "v${version}";
    sha256 = "sha256-GTEc9IqlfCW962/hXlEQ1ibXXbmjEfNXFVsuquUQdi8=";
  };
  modSha256 = "sha256-E59VhZQQrzEgbD+ZVHvO0Dq4ytYJIjG+V+629l4B+Y0=";
  vendorSha256 = "sha256-E59VhZQQrzEgbD+ZVHvO0Dq4ytYJIjG+V+629l4B+Y0=";

  meta = with lib; {
    description = "connects to utilitywarehouse vpn using wireguard";
    homepage = https://github.com/utilitywarehouse/wiresteward;
    platforms = platforms.all;
  };
}
