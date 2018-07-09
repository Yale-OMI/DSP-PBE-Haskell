# Synthesis Synthesis

The goal of this project is to build filter by providing example sound files

## Milestones

- [x] Compare aural 'distance' of two sound files
- [x] Use stochastic gradient descent to find a DSP filter to fit the examples
- [x] generate Vivid program from DSP filter
- [ ] Use refinement types to find better initial guess for SGD
- [ ] build online interface

## Usage

To install 

   pip install scipy numpy matplotlib 
   make

To test

   make test

or, to run the tests with a ramdisk
 
   ./runTests.sh

To run

   ./runAll.sh


