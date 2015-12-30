# nixrbd - NixOS Remote Boot Daemon
A daemon that serves NixOS clients booting over iPXE.

This project is under active development. It is perfectly usable right now, but
the feature set and command syntax can change without any notice. I will do my
best to keep this README up to date with the current code, but I make no
promises. There is a version number defined in the code, but it has no real
connection to the current state of the code. There are no proper releases yet.


## Build and install

It easy to build nixrbd if you have Nix set up properly. Run the following
command in the repository root:

```
nix-env -i -f . \
  -I nixpkgs=https://github.com/NixOS/nixpkgs-channels/archive/nixos-15.09.tar.gz
```

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

In the example above, the `menu-simple.nix` could look like this:

```
{ request, ... }:

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

### Routes
When launching nixrbd you can give it an arbitrary number of routes as `-r` or
`--route` command line options. Each route has the following form:

```
<request uri>,<response uri>
```

The request URI is relative the root (the initial `/` is optional), and nixrbd
will match every incoming requests against all specified routes. The most
specific route that still matches the request will be picked. Say you have the
following routes defined:

```
-r /foo,<B>
-r /,<A>
-r /foo?host=myhost,<C>
```

Then the requests would be handled as below:

```
/                    -> A
/foobar              -> A
/foo/bar             -> B
/foo?host=yourhost   -> B
/foo/bar?host=myhost -> C
```

The route target (the response URI) can be of tree types: static file, nix
build and empty response with pre-defined status code.

### Testing nixrbd together with iPXE
The `utils` folder in the repository root includes a test script that
automatically launches both a nixrbd instance and a qemu instances that boots
with an iPXE image that chain-loads the script that nixrbd serves. This way,
you can try out your boot scripts in a virtual environment. The following
example makes use of a sample menu (`menu-simple.nix`) that also is bundled
with nixrbd:

	utils/start_qemu -r /nix/store,file:///nix/store -r /,nix://`pwd`/utils/menu-simple.nix

All arguments to the script will be forwarded directly to `nixrbd`. The script
will build nixrbd, ipxe and qemu, and bring up your boot menu/script in a qemu
window.
