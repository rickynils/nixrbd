{ mkDerivation, base, bytestring, cmdargs, data-default, directory
, filepath, hslogger, http-types, mtl, network, network-uri
, process, split, stdenv, text, transformers, wai, wai-extra, warp
}:
mkDerivation {
  pname = "nixrbd";
  version = "0.1.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring cmdargs data-default directory filepath hslogger
    http-types mtl network network-uri process split text transformers
    wai wai-extra warp
  ];
  description = "NixOS Remote Boot Daemon";
  license = stdenv.lib.licenses.unfree;
}
