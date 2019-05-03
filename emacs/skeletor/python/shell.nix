with import <nixpkgs> {};
with pkgs.python3Packages;

mkShell rec {
  name = "__PROJECT-NAME__";

  # Customizable development requirements
  buildInputs = [
   (python36.withPackages(ps: with ps; [
	   __PACKAGES__
		]))
  ];
}
