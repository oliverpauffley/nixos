{ emacsPackages, fetchFromGitHub, }:
emacsPackages.trivialBuild rec {
  pname = "feature-mode";
  version = "latest";
  src = fetchFromGitHub {
    owner = "freesteph";
    repo = "cucumber.el";
    rev = "8d43c37ddf986af769870da27c31c1911f35b205";
    hash = "sha256-RNn9jQN3bQ1NETi9+VuSNcxBeQknkij1kEk8shMYWwI=";
  };
  # Manually copy the queries folder to the output path
  postInstall = ''
    cp -r $src/gherkin-languages.json $out/share/emacs/site-lisp/
  '';
}
