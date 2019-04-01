with import <nixpkgs> {};
mkShell rec {
    name = "__PROJECT-NAME__";
    buildInputs = [ sbcl ];
    LD_LIBRARY_PATH = pkgs.stdenv.lib.makeLibraryPath buildInputs;
}
