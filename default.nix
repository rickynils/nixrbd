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
    cp lib/*.nix $out/include/
  '';
  buildDepends = [
    cmdargs
    hslogger
    httpTypes
    text
    transformers
    wai
    waiLogger
    warp
  ];
  meta = {
    homepage = https://github.com/rickynils/nixrbd;
    description = "NixOS Remote Boot Daemon";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
    maintainers = [ self.stdenv.lib.maintainers.rickynils ];
  };
})
