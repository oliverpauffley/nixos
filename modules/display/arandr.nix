{
  # TODO this works for my laptop but not in general!
  flake.modules.homeManager.base = { pkgs, ... }: {
    programs.autorandr = let
      eDP-1-mode = "2160x1350";
      DVI-I-1-1-mode = "2560x1440";
      bigScreenDPI = 240;
      laptopDPI = 120;
    in {
      enable = true;
      profiles = {
        "work" = {
          fingerprint = {
            DVI-I-1-1 =
              "00ffffffffffff001e6d805bb42d0000041f0103803c2278ea8cb5af4f43ab260e5054254b007140818081c0a9c0b300d1c08100d1cf5aa000a0a0a0465030203a0055502100001a000000fd0030781ee63c000a202020202020000000fc004c4720554c545241474541520a000000ff003130344e54514430423730300a015a020344f1230907074d100403011f13123f5d5e5f5f5f6d030c001000b83c20006001020367d85dc401788003e30f0018681a00000101307800e305c000e6060000000000d97600a0a0a0345030203a0055502100001a565e00a0a0a029503020350055502100001a000000000000000000000000000000000000000000000011";
            eDP-1 =
              "00ffffffffffff000dae0113000000002c1d0104a51c1178eaee95a3544c99260f5054000000010101010101010101010101010101015046702c804611501010810018af100000185023702c804611501010810018af10000018000000fe00434d4e0a202020202020200113000000fe00503130315a465a2d424832202000e1";
          };
          config = {
            DVI-I-1-1 = {
              dpi = bigScreenDPI;
              enable = true;
              mode = "${DVI-I-1-1-mode}";
              position = "2160x0";
              primary = true;
              rate = "60.00";
            };
            eDP-1 = {
              dpi = laptopDPI;
              enable = true;
              mode = "${eDP-1-mode}";
              position = "0x0";
              rate = "60.00";
            };
          };
        };
        "default" = {
          fingerprint = {
            eDP-1 =
              "00ffffffffffff000dae0113000000002c1d0104a51c1178eaee95a3544c99260f5054000000010101010101010101010101010101015046702c804611501010810018af100000185023702c804611501010810018af10000018000000fe00434d4e0a202020202020200113000000fe00503130315a465a2d424832202000e1";
          };
          config = {
            eDP-1 = {
              dpi = laptopDPI;
              enable = true;
              mode = "${eDP-1-mode}";
              primary = true;
              position = "0x0";
              rate = "60.00";
            };
          };
        };
        "tosh" = {
          fingerprint = {
            DP-3 =
              "00ffffffffffff0010ac7240533232312c17010380331d78ea6ea5a3544f9f26115054a54b00714f8180d1c001010101010101010101023a801871382d40582c4500fe1f1100001e000000ff004b46383759334153313232530a000000fc0044454c4c205532333132484d0a000000fd00384c1e5311000a2020202020200042";
            eDP-1 =
              "00ffffffffffff000dae0113000000002c1d0104a51c1178eaee95a3544c99260f5054000000010101010101010101010101010101015046702c804611501010810018af100000185023702c804611501010810018af10000018000000fe00434d4e0a202020202020200113000000fe00503130315a465a2d424832202000e1";
          };
          config = {
            eDP-1 = {
              dpi = laptopDPI;
              enable = true;
              mode = "${eDP-1-mode}";
              primary = true;
              position = "2560x0";
              rate = "60.00";
            };
            DP-3 = {
              dpi = bigScreenDPI;
              enable = true;
              mode = "${DVI-I-1-1-mode}";
              position = "0x0";
              rate = "99.95";
            };
          };
        };
      };
      hooks = {
        postswitch = let dpi = builtins.toString laptopDPI;
        in {
          "change-dpi" = ''
            case "$AUTORANDR_CURRENT_PROFILE" in
              default)
                DPI=${dpi}
                ;;
              home)
                DPI=${dpi}
                ;;
              work)
                DPI=${dpi}
                ;;
              tosh)
                DPI=${dpi}
                ;;
              *)
                echo "Unknown profile: $AUTORANDR_CURRENT_PROFILE"
                exit 1
            esac

            echo "Xft.dpi: $DPI" | ${pkgs.xorg.xrdb}/bin/xrdb -merge
          '';
        };
      };
    };
  };
}
