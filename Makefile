all: .cabal-stamp src/OHSc/ohsc

.depends-stamp:
	./scripts/update-dependencies.sh
	touch .depends-stamp

.cabal-stamp: .depends-stamp
	cabal configure
	cabal build
	touch .all-stamp

src/OHSc/ohsc: .depends-stamp
	make -C ./src/OHSc all

clean:
	rm -f .depends-stamp .cabal-stamp
