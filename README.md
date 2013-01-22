# nixrbd - NixOS Remote Boot Daemon

A daemon that serves NixOS clients booting over iPXE.

This project is under initial development, and not usable for the time being.

## Build and install

You can use either Haskell Cabal or the Nix package manager to build and
install `nixrbd` from the source.

With `nix`, do this in the repository root:

	nix-env -i -A nixrbd -f pkgs.nix

to build and install `nixrbd` into your nix profile.

With `cabal`, do:

	cabal install

to install `nixrbd` among your other Haskell packages.

## Configure and run
