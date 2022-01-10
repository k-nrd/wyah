{ system ? builtins.currentSystem, compiler ? null }:
let
  pkgs = import ./nix { inherit system compiler; };
in
pkgs.mkShell {
  buildInputs = [
    pkgs.wyah.shell
  ];
  shellHook = ''
    export LD_LIBRARY_PATH=${pkgs.wyah.shell}/lib:$LD_LIBRARY_PATH
    logo
  '';
}
