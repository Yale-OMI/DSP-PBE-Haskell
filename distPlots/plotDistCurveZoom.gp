set xlabel "low-pass filter threshold (Hz)"
set ylabel "Aural Distance"
set key bottom left
plot "lpf800-zoom.csv" with lp
set term png size 1024,768
set output "distCurveZoom.png" 
replot
