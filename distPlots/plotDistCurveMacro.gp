set xlabel "low-pass filter threshold (Hz)"
set ylabel "Aural Distance"
set xrange [8000:21000]
set key bottom left
plot "lpf800-macro.csv" with lp
set term png size 1024,768
set output "distCurveMacro.png" 
replot
