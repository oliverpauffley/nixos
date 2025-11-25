{
  flake.modules.homeManager.base = { pkgs, config, lib, ... }:
    let
      # Modern Emacs with optimizations (primary configuration)
      emacs-base = pkgs.emacs-gtk.override {
        withNativeCompilation = true;
        withTreeSitter = true;
        withSQLite3 = true;
      };

      # Comprehensive package list following 2024-2025 best practices
      emacsPackages = epkgs:
        with epkgs; [
          # Core framework
          use-package
          diminish
          bind-key

          # Modern completion framework (Vertico ecosystem)
          vertico
          consult
          embark
          embark-consult
          marginalia
          orderless
          helpful

          # Completion UI (Ultimate Corfu setup)
          corfu
          corfu-terminal # Terminal support for Emacs 30
          popon # Required dependency for corfu-terminal
          cape
          kind-icon # Beautiful completion icons
          wgrep

          # keys
          meow
          # TODO install these
          # meow-vterm
          meow-tree-sitter
          which-key

          # LSP integration (Pure Corfu approach - no LSP-UI)
          lsp-mode
          consult-lsp
          lsp-haskell

          # Programming languages
          # formatting
          apheleia
          # elisp
          highlight-quoted
          highlight-defined
          suggest

          go-mode
          rust-mode
          web-mode
          nix-mode
          yaml-mode
          json-mode
          markdown-mode
          haskell-mode

          # Git integration
          magit
          magit-section
          git-timemachine
          forge
          git-link
          diff-hl

          # Project management
          projectile
          consult-projectile
          dashboard

          # Org mode
          org
          org-contrib
          org-cliplink
          elfeed
          elfeed-org
          elfeed-goodies

          # UI enhancements
          doom-modeline
          all-the-icons

          # Terminal integration
          vterm
          clipetty # OSC 52 clipboard support for universal clipboard integration

          # Utilities
          smartparens
          rainbow-delimiters
          ws-butler
          editorconfig
          flycheck
          yasnippet
          yasnippet-snippets

          # Tree-sitter grammars (from emacs-overlay)
          treesit-grammars.with-all-grammars
        ];

      # Package overrides for compatibility
      packageOverrides = self: super: {
        # Ensure we use the right org version
        org = super.elpaPackages.org;
      };

      # Build final Emacs with all packages
      myEmacs = ((pkgs.emacsPackagesFor emacs-base).overrideScope
        packageOverrides).emacsWithPackages emacsPackages;

      # Development tools and LSP servers
      devPackages = with pkgs; [
        # LSP servers and tools
        gopls
        nodePackages.eslint
        nixd
        nil # Alternative Nix LSP server

        # LSP Performance booster (2-10x faster LSP)
        emacs-lsp-booster

        # Formatters
        gofumpt
        nodePackages.prettier

        # Essential tools
        ripgrep
        fd
        sqlite
      ];

    in {
      # === PRIMARY CONFIGURATION: Nix-Vanilla (Modern Terminal-First Emacs) ===

      # Install nix-vanilla configuration files
      home.file.".config/emacs/nix-vanilla/init.el" = { source = ./init.el; };
      home.file.".config/emacs/nix-vanilla/config.org" = {
        source = ./config.org;
      };
      home.file.".config/emacs/nix-vanilla/early-init.el" = {
        source = ./early-init.el;
      };

      # === PACKAGES ===

      home.packages = devPackages ++ [ myEmacs ];

    };
}
