{
  flake.modules.nixos.base =
    let
      onePassPath = "~/Library/Group Containers/2BUA8C4S2C.com.1password/t/agent.sock";
    in
    {
      programs.ssh = {
        enable = true;
        extraConfig = ''
          Host *
              IdentityAgent ${onePassPath}
        '';
      };
      services.openssh = {
        enable = true;
        openFirewall = true;
        settings = {
          X11Forwarding = true;
          StreamLocalBindUnlink = "yes";
          PermitRootLogin = "yes";
        };
      };
    };
}
