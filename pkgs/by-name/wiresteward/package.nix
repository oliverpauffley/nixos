{ buildGoModule, fetchFromGitHub, lib, ... }:
buildGoModule rec {
  pname = "wiresteward";
  version = "0.2.7-RC1";
  doCheck = false;

  src = fetchFromGitHub {
    owner = "utilitywarehouse";
    repo = pname;
    rev = "v${version}";
    sha256 = "sha256-tVkXTzsjbvgzSMfh6brWoVVlMKF39oCKoHmuD+Vmy24=";
  };
  vendorHash = "sha256-8tXvGHrIt9xchFtd4j2QSQJFYMiKuRLsONdjTflsZ2I=";

  meta = with lib; {
    description = "connects to utilitywarehouse vpn using wireguard";
    homepage = "https://github.com/utilitywarehouse/wiresteward";
    platforms = platforms.all;
  };
}
