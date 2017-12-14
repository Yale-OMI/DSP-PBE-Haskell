NAME=tsl2tlsf
MAIN=src/Main.hs
BLDDIR=bld

default: 
	cabal install

test :
	cabal test --show-details=streaming

haddock:
	@cabal --executable --hyperlink-source haddock

