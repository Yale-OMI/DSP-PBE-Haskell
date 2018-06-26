"""
Takes a sound file, splits it into time slices, and builds FFT for each slice
"""

import sys
import pylab
from scipy.io import wavfile
from fft_util import timeSliceFFT
from slidingWindow import window

myAudio = sys.argv[1]
windowSize = int (sys.argv[2])
windowOverlap = int (sys.argv[3])

#Read file and get sampling freq [ usually 44100 Hz ]  and sound object
samplingFreq, mySound = wavfile.read(myAudio)

#Check if wave file is 16bit or 32 bit. 24bit is not supported
mySoundDataType = mySound.dtype

#We can convert our sound array to floating point values ranging from -1 to 1 as follows

mySound = mySound / (2.**15)

#Check sample points and sound channel for duel channel(5060, 2) or  (5060, ) for mono channel

mySoundShape = mySound.shape
samplePoints = float(mySound.shape[0])

#If two channels, then select only one channel
mySoundOneChannel = mySound[:,0]


for audioSlice in window(mySoundOneChannel, windowSize, windowOverlap):
  timeSliceFFT(list(audioSlice), samplingFreq)



