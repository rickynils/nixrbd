{ haskellPackages }:

with haskellPackages;

cabal.mkDerivation (self: {
  pname = "nixrbd";
  version = "0.0";
  isLibrary = false;
  isExecutable = true;
  src = ./.;
  buildDepends = [
    cmdargs
    httpTypes
    text
    transformers
    wai
    warp
  ];
})
