#!/bin/bash

subs=$(ls $(dirname $0)/../submodules/)
cd $(dirname $0)/..

for s in $subs; do
    cabal sandbox add-source submodules/$s
done
