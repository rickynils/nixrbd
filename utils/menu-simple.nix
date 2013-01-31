with import <nixpkgs> {};

writeText "menu.ipxe" ''
  #!ipxe

  :menu_root
  menu Select a boot alternative
  item memtest Run Memtest
  item shell   Enter iPXE shell
  choose target && goto ''${target} || goto menu_root

  :shell
  shell && goto menu_root

  :memtest
  chain http://boot.ipxe.org/memtest.0
''
