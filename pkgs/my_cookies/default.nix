{ buildPythonApplication, fetchgit, lib, browser-cookie3 }:
buildPythonApplication rec {
  pname = "my_cookies";
  version = "0.1.3";
  src = fetchgit {
    url = "https://github.com/kaiwk/my_cookies";
    hash = "sha256-9UXOrf9N1UZKY4LiclhTe5N6tHzM10j/n9MiaR1aGZE=";
  };
  propagatedBuildInputs = [ browser-cookie3 ];
  meta = with lib; {
    description = "handles getting cookies for leetcode.el in emacs";
    homepage = "https://github.com/kaiwk/my_cookies";
    platforms = platforms.all;
  };
}
