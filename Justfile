install NAME:
    sudo nixos-rebuild switch --flake .#{{NAME}}

darwin NAME:
    sudo darwin-rebuild switch --flake .#{{NAME}}

upgrade NAME:
    nh os switch . -u -H {{NAME}}
    
clean:
    nh clean all -k 2
