with import <nixpkgs> {};

let

  rewriteNixRoot = newRoot: path: 
    newRoot + (builtins.substring 4 (builtins.stringLength path) path);

in writeText "menu.ipxe" ''
  #!ipxe

  :menu_root
  menu Select a boot alternative
  item memtest Run Memtest
  item shell   Enter iPXE shell
  choose target && goto ''${target} || goto menu_root

  :shell
  shell && goto menu_root

  :memtest
  chain ${rewriteNixRoot "http://10.0.2.2:8000" "${memtest86plus}/memtest.bin"}
''
