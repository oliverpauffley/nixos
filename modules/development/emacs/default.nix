{
  flake.modules.homeManager.base = { pkgs, config, lib, ... }:
    let
      # Modern Emacs with optimizations (primary configuration)
      emacs-base = pkgs.emacs-gtk.override {
        withNativeCompilation = true;
        withTreeSitter = true;
        withSQLite3 = true;
      };

      emacsPackages = epkgs:
        with epkgs; [
          # Core framework
          use-package
          minions
          bind-key
          pdf-tools
          envrc

          # Modern completion framework (Vertico ecosystem)
          vertico
          consult
          consult-gh
          consult-gh-embark
          consult-gh-forge
          consult-gh-with-pr-review
          consult-yasnippet
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
          ind-icon # Beautiful completion icons
          wgrep

          # keys
          meow
          pkgs.local.meow-treesitter
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

          # Tree-sitter grammars (from emacs-overlay)
          tree-sitter-langs
          (treesit-grammars.with-grammars (p: [
            p.tree-sitter-bash
            p.tree-sitter-dockerfile
            p.tree-sitter-elisp
            p.tree-sitter-markdown
            p.tree-sitter-markdown-inline
            p.tree-sitter-nix
            p.tree-sitter-python
            p.tree-sitter-rust
            p.tree-sitter-toml
            p.tree-sitter-yaml
            p.tree-sitter-go
            p.tree-sitter-gowork
            p.tree-sitter-gomod
            p.tree-sitter-json
            p.tree-sitter-sql
            p.tree-sitter-just
            p.tree-sitter-haskell
            p.tree-sitter-proto
          ]))

          go-mode
          rust-mode
          web-mode
          nix-mode
          yaml-mode
          json-mode
          markdown-mode
          protobuf-ts-mode
          consult-hoogle

          exercism

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
          perspective
          persp-projectile

          # Org mode
          epkgs.org
          ox-hugo
          ox-gfm

          org-contrib
          org-contacts
          org-cliplink
          org-modern
          elfeed
          elfeed-org
          elfeed-goodies

          hyperbole

          # UI enhancements
          doom-modeline
          all-the-icons
          all-the-icons-dired

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
          eshell-z

          # My packages
          pkgs.local.vaarn
        ];

      packageOverrides = self: super: { org = super.elpaPackages.org; };

      # Build final Emacs with all packages
      myEmacs = ((pkgs.emacsPackagesFor emacs-base).overrideScope
        packageOverrides).emacsWithPackages emacsPackages;

      tex = (pkgs.texlive.combined.scheme-full.withPackages (ps:
        with ps; [
          dvisvgm
          dvipng # for preview and export as html
          wrapfig
          amsmath
          ulem
          hyperref
          capt-of
        ]));
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
        silver-searcher

        # latex
        tex
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
      home.file.".config/emacs/nix-vanilla/org/autoloads.el" = {
        source = ./org/autoloads.el;
      };

      # === PACKAGES ===

      home.packages = devPackages ++ [ myEmacs ];

      xdg.desktopEntries.vanilla-emacs = {
        name = "Vanilla Emacs";
        genericName = "Emacs with nix";
        exec = "emacs --init-directory .config/emacs/nix-vanilla/";
        terminal = false;
        categories = [ "System" ];
        mimeType = [ "text/org" ];
      };
    };
}
