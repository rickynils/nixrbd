{ src ? ./., haskellPackages }: haskellPackages.buildLocalCabal src "nixrbd"
