with import <nixpkgs> {};
mkShell rec {
    name = "__PROJECT-NAME__";
    buildInputs = [__PACKAGES__];
    # LD_LIBRARY_PATH = pkgs.stdenv.lib.makeLibraryPath buildInputs;
}
