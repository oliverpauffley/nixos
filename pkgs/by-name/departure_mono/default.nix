{ lib, stdenvNoCC, fetchzip }:

stdenvNoCC.mkDerivation {
  pname = "departure_mono";
  version = "v1.346";

  src = fetchzip {
    url = "https://departuremono.com/assets/DepartureMono-1.346.zip";
    hash = "sha256-xJVVtLnukcWQKVC3QiHvrfIA3W9EYt/iiphbLYT1iMg=";
    stripRoot = false;
  };

  installPhase = ''
    runHook preInstall

    install -Dm644 *.otf -t $out/share/fonts/truetype

    runHook postInstall
  '';

  meta = with lib; {
    description =
      "Departure mono is monospaced pixel font with  a lo-fi technical vibe";
    license = licenses.ofl;
  };
}
