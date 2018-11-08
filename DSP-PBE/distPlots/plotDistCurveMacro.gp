set xlabel "low-pass filter threshold (Hz)"
set ylabel "Aural Distance"
set xrange [8000:21000]
set key bottom left
plot "lpf800-macro.csv" with lp
set term tikz color size 3.5in,3.5in 
set output "distCurveMacro.tex" 
replot
