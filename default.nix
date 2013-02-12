{ haskellPackages }:

with haskellPackages;

cabal.mkDerivation (self: {
  pname = "nixrbd";
  version = "0.0";
  isLibrary = false;
  isExecutable = true;
  src = ./.;
  postInstall = ''
    mkdir -p $out/include
    cp lib/nixrbd.nix $out/include/
  '';
  buildDepends = [
    cmdargs
    httpTypes
    text
    transformers
    wai
    warp
  ];
})
