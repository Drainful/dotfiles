with import <nixpkgs> {};
mkShell rec {
  name = "__PROJECT-NAME__";

  buildInputs = [
    (pkgs.haskellPackages.ghcWithHoogle (ps: with ps; [
	    cabal-install
      __HASKELL-PACKAGES__
    ]))

		__PACKAGES__
  ];
  # LD_LIBRARY_PATH = pkgs.stdenv.lib.makeLibraryPath buildInputs;
}
