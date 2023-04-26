{ config, lib, pkgs, ... }:
{
  programs.autorandr = {
    enable = true;
    profiles = {
      "laptop" = {
        fingerprint = {
          "eDP-1" = "00ffffffffffff0030e4080600000000001c0104a51f117802e085a3544e9b260e5054000000010101010101010101010101010101012e3680a070381f403020350035ae1000001a542b80a070381f403020350035ae1000001a000000fe004c4720446973706c61790a2020000000fe004c503134305746392d5350463100d5";
        };
        config = {
          "eDP-1" = {
            enable = true;
            primary = true;
            position = "0x0";
            mode = "1920x1080";
            #gamma = "1.0:0.909:0.833";
            #rate = "60.00";
          };
        };
      };

      "laptop-dual" = {
        fingerprint = {
          "eDP-1" = "00ffffffffffff0030e4080600000000001c0104a51f117802e085a3544e9b260e5054000000010101010101010101010101010101012e3680a070381f403020350035ae1000001a542b80a070381f403020350035ae1000001a000000fe004c4720446973706c61790a2020000000fe004c503134305746392d5350463100d5";
          "HDMI-1" = "00ffffffffffff000469a52401010101201a010380351e78ea9de5a654549f260d5054b7ef00714f8180814081c081009500b3000101023a801871382d40582c4500132b2100001e000000fd00324c1e5311000a202020202020000000fc0056473234380a20202020202020000000ff0047384c4d51533030303233300a01dc02031ef14b900504030201111213141f230907078301000065030c0010001a3680a070381e4030203500132b2100001a662156aa51001e30468f3300132b2100001e011d007251d01e206e285500132b2100001e8c0ad08a20e02d10103e9600132b21000018011d8018711c1620582c2500132b2100009e000000000000003a";
        };
        config = {
          "eDP-1" = {
            enable = true;
            primary = true;
            position = "0x0";
            mode = "1920x1080";
            #gamma = "1.0:0.909:0.833";
            #rate = "60.00";
          };

          "HDMI-1" = {
            enable = true;
            primary = false;
            position = "1920x0";
            mode = "1920x1080";
            #gamma = "1.0:0.909:0.833";
            #rate = "60.00";
          };
        };
      };
    };
  };

}
