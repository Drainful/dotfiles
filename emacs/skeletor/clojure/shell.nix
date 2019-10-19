with import <nixpkgs> {};
mkShell rec {
    name = "__PROJECT-NAME__";
    buildInputs = [clojure];
}
