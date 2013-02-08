{ request, ... }:

with builtins;
with import <nixpkgs> {};

let

  mkIpxe = s: writeText "script.ipxe" "#!ipxe\n${s}";

  path = if request.pathInfo == [] then "" else head request.pathInfo;

  handlers = {
    memtest = mkIpxe "chain ${memtest86plus}/memtest.bin";

    default = mkIpxe ''
      :menu_root
      menu Select a boot alternative
      item memtest Run Memtest
      item iso     Run Slitaz Live CD
      item shell   Enter iPXE shell
      choose target && goto ''${target} || goto menu_root

      :shell
      shell && goto menu_root

      :memtest
      chain /memtest
    '';
  };

in if hasAttr path handlers then getAttr path handlers else handlers.default
