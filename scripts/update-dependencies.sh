#!/bin/sh

cd $(dirname $0)/..

export PATH="$PWD/.cabal-sandbox/bin:$PATH"

git submodule init
git submodule update
git submodule foreach git reset --hard HEAD
git submodule foreach git pull origin master

rm -f .cabal-sandbox/add-source-timestamps

[ ! -e cabal.sandbox.config -o ! -e .cabal-sandbox ] && cabal sandbox init

$(dirname $0)/add-sources.hs

#[ ! -e .cabal-sandbox/bin/gtk2hsC2hs ] && cabal install gtk2hs-buildtools
#[ ! -e .cabal-sandbox/bin/c2hs ] && cabal install c2hs
cabal install --enable-tests --only-dependencies --reinstall --force-reinstalls
cabal configure --enable-tests
