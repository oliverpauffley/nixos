#!/usr/bin/env nu

def main [] {}

def "main install" [name: string] {
  sudo nixos-rebuild switch --flake .#($name)
}

def "main darwin" [machine: string] {
  sudo darwin-rebuild switch --flake .#($machine)
}

def "main clean" [] {
  nh clean all -k 2
}
