{ request, ... }:

with import <nixpkgs> {};
with callPackage ../lib/nixrbd.nix {};

let 

  handler = mkDefaultMenu {
    memtest = {
      title = "Run Memtest";
      script = "chain ${memtest86plus}/memtest.bin";
    };
  
    nixos = {
      title = "Run Nixos";
      script = mkNixosBootEntry (
        import <nixos> {
          nixpkgs = <nixpkgs>;
          configuration = <configuration.nix>;
        }
      );
    };
  };

in handler request
