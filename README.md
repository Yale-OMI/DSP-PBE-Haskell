# Synthesis Synthesis

The goal of this project is to build filter by providing example sound files

## Milestones

- [x] Compare aural 'distance' of two sound files
- [ ] Use stochastic gradient descent to find a DSP filter to fit the examples
- [ ] generate Vivid program from DSP filter
- [ ] Use refinement types to find better initial guess for SGD
- [ ] build online interface

## Usage

To install 
 
   make

To test

   make test

or, to run the tests with a ramdisk
 
   ./runTests.sh

To run

   dist/build/musicSynth/musicSynth Sounds/SynthesisBenchmarks/Constructed/cartoon010.wav Sounds/SynthesisBenchmarks/Constructed/cartoon010-lpf800.wav 

## TODO Notes

  make plot of cost vs high pass filter threshold on lpf examples (really hope this is a flat curve...)
