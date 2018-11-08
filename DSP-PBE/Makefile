NAME=tsl2tlsf
MAIN=src/Main.hs
BLDDIR=bld

default: 
	cabal new-build

test: default
	cabal new-test --show-details=streaming

haddock:
	@cabal --executable --hyperlink-source haddock

