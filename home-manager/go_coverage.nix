{ pkgs }:
pkgs.writeShellScriptBin "go-test-coverage" ''
   t="/tmp/go-cover.$$.tmp"
  ${pkgs.unstable.go}/bin/go test -coverprofile=$t $@ && ${pkgs.unstable.go}/bin/go tool cover -html=$t && unlink $t
''
