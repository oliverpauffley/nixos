# User config applicable to both nixos and darwin
{ inputs, pkgs, config, lib, ... }:
let pubKeys = lib.filesystem.listFilesRecursive ./keys;
in {
  # These get placed into /etc/ssh/authorized_keys.d/<name> on nixos
  users.users.ollie.openssh.authorizedKeys.keys =
    lib.lists.forEach pubKeys (key: builtins.readFile key);
}
