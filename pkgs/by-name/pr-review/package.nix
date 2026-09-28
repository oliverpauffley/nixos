{ emacsPackages, fetchFromGitHub }:
emacsPackages.melpaBuild rec {
  pname = "pr-review";
  version = "0.1";

  src = fetchFromGitHub {
    owner = "blahgeek";
    repo = "emacs-pr-review";
    rev = "938db766007f3444a2899b2457d9e2f4b4ffbebf";
    hash = "sha256-YyO+HxWkbznnGV3xFoP9qz6KcoTakVWQ/C7DQDu8gWU";
  };

  packageRequires = with emacsPackages; [
    magit
    magit-section
    ghub
    markdown-mode
  ];

  postInstall = ''
    package_dir="$out/share/emacs/site-lisp/elpa/pr-review-${version}"
    mkdir -p "$package_dir"
    cp -r graphql "$package_dir/"
  '';
}
