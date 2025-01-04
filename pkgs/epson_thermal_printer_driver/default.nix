{ lib, stdenv, fetchzip, cups, pkg-config, cmake, tree }:

stdenv.mkDerivation rec {
  pname = "epson-tm-t88VII-driver";
  version = "3.0.0.0";

  src = fetchzip {
    url =
      "https://download3.ebz.epson.net/dsc/f/03/00/15/35/42/b1a708bb8b21d7a68ae7394287db440974b68a0e/tmx-cups-src-ImpactReceipt-3.0.0.0_pck_e.zip";
    sha256 = "sha256-c6VpnNXYebkDkK9kcTZ/ILE8pD/qSWKCHYqkHV+WIkc=";
  };

  buildInputs = [ cups pkg-config tree ];
  nativeBuildInputs = [ cmake ];

  unpackPhase = ''
    runHook preUnpack

    mkdir build
    tar -zxf $src/tmx-cups-src-ThermalReceipt-${version}.tar.gz --strip-components=1 -C .
    tree
    for ppd in ppd/* ; do
      substituteInPlace $ppd --replace "rastertotmtr" "$out/lib/cups/filter/rastertotmtr"
    done
    runHook postUnpack
  '';

  installPhase = ''
    cd ..
    mkdir -p $out/bin
    mkdir -p $out/lib/cups/filter
    mkdir -p $out/share/cups/model/epson-tm-thermal
    mkdir -p $out/etc/cups/ppd

    cp -a ppd/* $out/share/cups/model/epson-tm-thermal
    cp -a ppd/* $out/etc/cups/ppd
    cp build/rastertotmtr $out/bin
    cp build/rastertotmtr $out/lib/cups/filter
  '';

  meta = with lib; {
    description = "Epson TM-T88VII Driver";
    homepage = "https://www.epson.com/";
    # license = licenses.mit; # Adjust as necessary
    maintainers = with maintainers; [ opauffley ];
  };
}
