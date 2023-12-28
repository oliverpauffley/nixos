{ config, lib, pkgs, inputs, ... }:
let
  # Seems it reject missing fields.
  # https://github.com/rustsec/rustsec/blob/5058319167c0a86eae7bf25ebc820a8eefeb1c55/cargo-audit/audit.toml.example
  cargoAudit = {
    database = {
      path = "${config.xdg.cacheHome}/cargo/advisory-db";
      url = "https://github.com/RustSec/advisory-db.git";
      fetch = true;
      stale = false;
    };
  };

  # `--no-rosegment` is required for flamegraph
  # https://github.com/flamegraph-rs/flamegraph#cargo-flamegraph
  gcc-lld = pkgs.writeShellScript "gcc-lld" ''
    export PATH="${pkgs.llvmPackages_latest.bintools}/bin''${PATH:+:}$PATH"
    exec ${lib.getExe pkgs.gcc} -fuse-ld=lld -Wl,--no-rosegment "$@"
  '';
in {
  home.packages = with pkgs;
    with inputs.rust-overlay.packages.${pkgs.system}; [
      (lib.hiPrio rust-nightly.availableComponents.rustfmt)
      (rust.override {
        extensions = [ "rust-src" ];
        targets = [ "x86_64-unknown-linux-gnu" ];
      })

      pkg-config
      openssl
      openssl.dev
      cargo-audit
      cargo-bloat
      cargo-flamegraph
      cargo-hack
      cargo-insta
      cargo-license
      cargo-machete
      cargo-outdated
      cargo-show-asm
      rust-analyzer
    ];
}
