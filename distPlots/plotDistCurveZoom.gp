set xlabel "low-pass filter threshold (Hz)"
set ylabel "Aural Distance"
set key bottom left
plot "lpf800-zoom.csv" with lp
set term tikz color size 3.5in,3.5in 
set output "distCurveZoom.tex" 
replot
