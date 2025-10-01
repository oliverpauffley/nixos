install NAME:
    nh os switch . -H {{NAME}}

upgrade NAME:
    nh os switch . -u -H {{NAME}}

servers:
    colmena apply --impure

clean:
    nh clean all -k 2
