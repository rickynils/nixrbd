with import <nixpkgs> {};

writeText "menu.ipxe" ''
  #!ipxe

  :menu_root
  menu Please select a boot alternative
  item menu_sub    Submenu
  item
  item shell       Enter iPXE shell
  choose target && goto ''${target}

  :shell
  shell && goto menu_root

  :menu_sub
  menu Boot menu
  item menu_root      Go back to main menu
  choose target && goto ''${target}
''
