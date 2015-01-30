OHS{c,d,m}
==========

Compiling (Debian Jessie)
-------------------------

You'll need Debian 8 (Jessie) for this to work.

```
# apt-get install libwebkit-dev libsoup2.4-dev valac-0.26 ghc cabal-install
$ cabal update && cabal install Cabal
$ make
```

Running
-------

```
./dist/build/ohsd/ohsd &
./src/OHSc/ohsc localhost 1234
```
