# Synthesis Synthesis

The goal of this project is to build filter by providing example sound files

## Milestones

- [x] ability to compare audio 'closeness' of two sound files (TODO add writeup on this)
- [ ] generate candidate audio filtering programs
- [ ] build online interface

## Usage

To install 
 
   make

To test

   make test

To run

   dist/build/musicSynth/musicSynth Sounds/SynthesisBenchmarks/Constructed/cartoon010.wav Sounds/SynthesisBenchmarks/Constructed/cartoon010-lpf800.wav 

## TODO Notes

download audacity and check the FFT results - really seems like it isnt working correctly...
no need, just plotted with gnuplot - definitely wrong. Might need to try to use the audacity api, or call vivid...
I mean, its not completly wrong. somehow it is getting distances correct and roughly can find filters with SGD...
