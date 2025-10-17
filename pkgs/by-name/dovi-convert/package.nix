{ lib, makeWrapper, runCommand, pkgs, ... }:

let
  src = ./script.sh;
  binName = "dovi-convert";
  deps = with pkgs; [ mediainfo jellyfin-ffmpeg bash ];
in runCommand "${binName}" {
  nativeBuildInputs = [ makeWrapper ];
  meta = { mainProgram = "${binName}"; };
} ''
  mkdir -p $out/bin
  install -m +x ${src} $out/bin/${binName}

  wrapProgram $out/bin/${binName} \
    --prefix PATH : ${lib.makeBinPath deps}
''
