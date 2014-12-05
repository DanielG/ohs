#!/bin/sh

cd $(dirname $0)/..

git submodule init
git submodule foreach git fetch
git submodule update --recursive --checkout --force

[ ! -e cabal.sandbox.config -o ! -e .cabal-sandbox ] && cabal sandbox init

$(dirname $0)/add-sources.sh

[ ! -e .cabal-sandbox/bin/gtk2hsC2hs ] && cabal install gtk2hs-buildtools
[ ! -e .cabal-sandbox/bin/c2hs ] && cabal install c2hs
cabal install --enable-tests --only-dependencies --reinstall
