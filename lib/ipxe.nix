{ stdenv, fetchgit
, perl, syslinux, cdrkit, which, coreutils
, build ? "ipxe.iso", embeddedScript ? null
}:

with stdenv.lib;

stdenv.mkDerivation {
  name = "ipxe-git.${build}";

  src = fetchgit {
    url = "git://git.ipxe.org/ipxe.git";
    rev = "7405685df2bea9a457970d8b5a63ede08fcda6f7";
    sha256 = "0vs9js1x6ff58hw0xm7s0ym476bsvpr7g6mhg9sd65vpmdic4ysa";
  };

  buildInputs = [ perl syslinux cdrkit which coreutils ];

  phases = [ "unpackPhase" "buildPhase" "installPhase" ];

  buildPhase = ''
    cd src
    substituteInPlace util/geniso --replace 'cp -p $isolinux_bin $dir' \
      'cp $isolinux_bin $dir; chmod u+w $dir/isolinux.bin'
    make \
      ${optionalString (embeddedScript != null) "EMBED=${embeddedScript}"} \
      ISOLINUX_BIN=${syslinux}/share/syslinux/isolinux.bin \
      bin/${build}
  '';

  installPhase = ''
    cp bin/${build} $out
  '';
}
