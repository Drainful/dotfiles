with import <nixpkgs> {};
with pkgs.python3Packages;

buildPythonPackage rec {
  pname = "__PROJECT-NAME__";

  version = "0.0.1";

  buildInputs = with self; [];

  propogatedBuildInputs = with self; [];

  meta = {
    description = "__DESCRIPTION__";
  };
}
