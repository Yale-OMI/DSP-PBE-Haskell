NAME=tsl2tlsf
MAIN=src/Main.hs
BLDDIR=bld

default: 
	cabal build

test: default
	cabal test --show-details=streaming

haddock:
	@cabal --executable --hyperlink-source haddock

