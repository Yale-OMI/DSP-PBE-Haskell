
from scipy.fftpack import fft
import numpy
import matplotlib.pyplot as plt
import math

def timeSliceFFT(sound, samplingFreq):
  
  soundLength = len(sound)
 
  fftArray = fft(sound)

  numUniquePoints = int(numpy.ceil((soundLength + 1) / 2.0))
  fftArray = fftArray[0:numUniquePoints]

  #FFT contains both magnitude and phase and given in complex numbers in real + imaginary parts (a + ib) format.
  #By taking absolute value , we get only real part
  fftArray = abs(fftArray)

  #Scale the fft array by length of sample points so that magnitude does not depend on
  #the length of the signal or on its sampling frequency
  fftArray = fftArray / float(soundLength)

  #FFT has both positive and negative information. Square to get positive only
  fftArray = fftArray **2

  #Multiply by two (research why?)
  #Odd NFFT excludes Nyquist point
  if soundLength % 2 > 0: #we've got odd number of points in fft
    fftArray[1:len(fftArray)] = fftArray[1:len(fftArray)] * 2

  else: #We've got even number of points in fft
    fftArray[1:len(fftArray) -1] = fftArray[1:len(fftArray) -1] * 2  

  freqArray = numpy.arange(0, numUniquePoints, 1.0) * (samplingFreq / soundLength);

  #If this is ever the case, something went wrong
  if not len(freqArray) == len(fftArray):
    exit(1)

  for freq, amp in zip(freqArray, fftArray):
    if not math.isnan(amp):
      print(str(freq) + ", " + str(amp))

'''
  #Plot the frequency
  plt.plot(freqArray/1000, 10 * numpy.log10 (fftArray), color='B')
  plt.xlabel('Frequency (Khz)')
  plt.ylabel('Power (dB)')
  plt.show()
'''
