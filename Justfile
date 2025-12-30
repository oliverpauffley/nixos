install NAME:
    sudo nixos-rebuild switch --flake .#{{NAME}}

upgrade NAME:
    nh os switch . -u -H {{NAME}}

servers:
    colmena apply --impure

clean:
    nh clean all -k 2
