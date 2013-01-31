{ imageType ? "iso", embeddedScript ? null }:

with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "ipxe-git.${imageType}";

  src = fetchgit {
    url = "git://git.ipxe.org/ipxe.git";
    rev = "3fcb8cf8dceb45f8b01e1d69d43cfd99df43b78a";
    sha256 = "11b15myz7s3vgk2lja95c5cvgf2yhcjjyhy0ka2zij5zlyivjm4d";
  };

  buildNativeInputs = [ perl syslinux cdrkit which coreutils ];

  phases = [ "unpackPhase" "buildPhase" "installPhase" ];

  buildPhase = ''
    cd src
    substituteInPlace util/geniso --replace 'cp -p $isolinux_bin $dir' \
      'cp $isolinux_bin $dir; chmod u+w $dir/isolinux.bin'
    make \
      ${lib.optionalString (embeddedScript != null) "EMBED=${embeddedScript}"} \
      ISOLINUX_BIN=${syslinux}/share/syslinux/isolinux.bin \
      bin/ipxe.${imageType}
  '';

  installPhase = ''
    cp bin/ipxe.${imageType} $out
  '';
}