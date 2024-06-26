{ pkgs }:
pkgs.writeShellScriptBin "git-visualize" ''
  ${pkgs.gource}/bin/gource --auto-skip-seconds 0.1 -s 0.1 --output-ppm-stream - | ${pkgs.ffmpeg}/bin/ffmpeg -y -r 30 -f image2pipe -vcodec ppm -i - -b 65536K movie.mp4
''
