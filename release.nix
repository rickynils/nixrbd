{ pkgs ? import <nixpkgs> {}
, src ? ./.
}:

with pkgs;

{

  build = callPackage ./default.nix { inherit src; };

}
