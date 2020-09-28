# Nixos Configuration files
This repo contains all the configuration files needed to create my nixos environment. Including dotfiles for i3, polybar, rofi and more.

## Installation
To start the setup first clone the repo to .config/ and run
```bash
sudo nixos-rebuild switch -I nixos-config=.config/configuration.nix
```
To create the main configuration setup.

Then to install the home.nix files for home-manager
```bash
home-manager switch
```
and everything should be setup and running after a reboot.

