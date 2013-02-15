with (import <nixpkgs> {});

{
  ipxe = callPackage ../lib/ipxe.nix {
    imageType = "iso";
    embeddedScript = ./chain.ipxe;
  };
}
