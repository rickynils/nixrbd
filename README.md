# nixrbd - NixOS Remote Boot Daemon
A daemon that serves NixOS clients booting over iPXE.

This project is under initial development, and not usable for the time being.


## Build and install

It easy to build nixrbd if you have Nix set up properly. Run the following
command in the repository root:

	nix-env -i -A nixrbd -f pkgs.nix

This will install nixrbd in your Nix profile and make it available in your
path.


## Configure and run
nixrbd is really an HTTP server that lets you use Nix expressions to handle
requests. Strictly speaking, you can use it to serve whatever content you'd
like over HTTP, not only iPXE scripts. But since this project is about remote
booting, it aims to also provide tools and libraries to make it easy to build
custom boot environments on top of the core nixrbd server.

### Basic usage
You need a working Nix setup to be able to run nixrbd. The Nix utilities
(`nix-instantiate`, `nix-store`, etc) need to be in the path when running
nixrbd.

The following command launches nixrbd with a single request handler
(`menu-simple.nix`):

	nixrdb -r /,nix:///absolute/path/to/menu-simple.nix

nixrbd will now run in the foreground and listen for HTTP requests on its
default port 8000.

A request handler is a Nix expression that should be buildable with
`nix-build`, and must produce a single file as its output. On an incoming HTTP
request, nixrbd will build the expression and serve the resulting file to the
HTTP client.

In the example above, the `menu-simple.nix` can be found in the `utils`
directory and has the following content:

```
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
```

This creates a simple menu with two options: either boot to the iPXE shell, or run memtest.

You can now make an HTTP request and verify that nixrbd correctly builds and
serves your Nix expression:

```
$ curl http://localhost:8000
#!ipxe

:menu_root
menu Select a boot alternative
item memtest Run Memtest
item shell   Enter iPXE shell
choose target && goto ${target} || goto menu_root

:shell
shell && goto menu_root

:memtest
chain http://boot.ipxe.org/memtest.0
```

### Testing nixrbd together with iPXE
The `utils` folder in the repository root includes a test script that automatically launches both a nixrbd instance and a qemu instances that boots with an iPXE image that chain-loads the script that nixrbd serves. This way, you can try out your boot scripts in a virtual environment. The following example makes use of a sample menu (`menu-simple.nix`) that also is bundled with nixrbd:

	utils/start_qemu -r /nix/store,file:///nix/store -r /,nix://`pwd`/utils/menu-simple.nix

All arguments to the script will be forwarded directly to `nixrbd`. The script will build nixrbd, ipxe and qemu, and bring up your boot menu/script in a qemu window.
