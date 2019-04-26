unset grid
set terminal pdfcairo
set output "wykres_kind8.pdf"
set key box top left
set multi
set logscale x
set logscale y
set key opaque
set key box
set key width 1 height 0.5 font  'Arial, 14' 
set style data lines
set termopt enhanced
set xlabel 'N' font 'Arial,14'
set ylabel 'multiplication time' font 'Arial,14'
set xtics font 'Arial,14'
set ytics font 'Arial,14'
set termoption dashed

plot "naive_kind8" title "naive"
replot "better_kind8" title "better"
replot "dot_kind8" title "dot"
replot "mat_kind8" title "matmul"

unset multi
