# Synthesis Synthesis

The goal of this project is to build filter by providing example sound files

to read more on this project, see http://marksantolucito.com/dsp-pbe.pdf

## Milestones

- [x] Compare aural 'distance' of two sound files
- [x] Use stochastic gradient descent to find a DSP filter to fit the examples
- [x] generate Vivid program from DSP filter
- [ ] output generated program in runable form
- [ ] Use refinement types to find better initial guess for SGD
- [ ] build online interface

## Usage

To install 

   sudo pip install scipy numpy matplotlib
   sudo apt-get install python-tk supercollider
   scide #supercollider needs to be opened and closed once in order to initialize some folders that are used by the vivid api
   cabal sandbox init
   cabal new-install

To test

  cabal new-test 

To test one benchmark

  cabal new-run farm_benchmarks -- --color=always

To run the executable

  .cabal-sandbox/bin/musicSynth input-file output-file target-file
  
## Inputs

must be stereo tracks at 44.1k
