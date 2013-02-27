{ request, ... }:

with import <nixpkgs> {};
with callPackage ../lib/nixrbd.nix {};

let 

  handler = {
    default = mkMenu request {
      memtest = "Run Memtest";
      nixos = "Run Nixos";
    };

    memtest = mkIpxe "chain ${memtest86plus}/memtest.bin";
  
    nixos = mkNixosBootScript (
      import <nixos> {
        nixpkgs = <nixpkgs>;
        configuration = <configuration.nix>;
      }
    );
  };

in mkHandler request handler
