# Mainline
This program attempts to make an index of the meta info available on the
distributed hash table used by bittorrent (aka Mainline DHT)

## Installation

Currently this program is built and tested under NixOS. It may build on other
systems running the Nix package manager, which you can install with

```bash
curl https://nixos.org/nix/install | sh
```

To build the application run `nix-build shell.nix`

Then modify settings.json and execute: `./result/bin/Mainline settings.json`

If you are building on other systems see shell.nix for a full
list of dependencies. (You need ghc and some haskell packages, two
of which are not (yet) pushed upstream and need to be built from source.
I recommend using Nix.)

## Functions

 - Implements [bep_0005](http://www.bittorrent.org/beps/bep_0005.html)
   and only enough [bep_0003](http://www.bittorrent.org/beps/bep_0003.html)
   and [bep_0010](http://www.bittorrent.org/beps/bep_0010.html)
   to get meta info via
   [bep_0009](http://www.bittorrent.org/beps/bep_0009.html).
 - Can multiplex multiple nodes on a single port.
 - Can use multiple ports
 - Captures and writes metadata to PostgreSQL (see sql/sql.sql)

## Overview

The "Architecture" directory is what evolved into a network client framework,
based on the programming pattern of [The Elm Architecture](https://guide.elm-lang.org/architecture/)
encouraged by Elm lang. It makes dealing with state explicit and helped
simplifiy the code quite a bit. In short it takes care of opening and closing
network sockets and allows for a composable program architecture that keeps
the state consistent. For a complete usage example see Hello.hs

Mainline.hs implements a mainline dht node that talks to other nodes via KRPC.
App.hs implements multiplexing multiple nodes on one port and intercepts
various messages to initiate finding and downloading metadata.

ResolveMagnet.hs allows downloading meta info from the bittorrent network (once
you have peer information)
