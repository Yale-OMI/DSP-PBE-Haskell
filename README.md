[![Build Status](https://travis-ci.com/Yale-OMI/DSP-PBE.svg?branch=master)](https://travis-ci.com/Yale-OMI/DSP-PBE)

# Synthesis Synthesis

The goal of this project is to build a filter by providing example sound files

To read more on this project, see http://marksantolucito.com/dsp-pbe.pdf, or any of the ongoing writing in the 'papers' directory

## Usage

To install, run the following commands (taken from the [travis build file](.travis.yml)). This will take a good while

```
   sudo add-apt-repository -y ppa:hvr/ghc
   sudo apt-get update
   sudo apt-get install cabal-install-$CABALVER ghc-$GHCVER
   export PATH=/opt/ghc/$GHCVER/bin:/opt/cabal/$CABALVER/bin:$PATH
   sudo apt-get install python-tk supercollider
   sudo pip install scipy numpy matplotlib
   xvfb-run --server-args="-screen 0, 1280x800x24" sclang & sleep 5 ; kill %1
   cd DSP-PBE
   cabal --version
   ghc --version
   cabal update
   cabal sandbox init
   cabal install
```

To run the test suite

```
   cabal new-test
```

To run the executable

```
  .cabal-sandbox/bin/musicSynth input-file output-file target-file
```
 
or

```
cabal new-run farm_benchmarks -- --color-always
```

## Inputs

must be stereo tracks at 44.1k
