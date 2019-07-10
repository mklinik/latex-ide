$pdf_mode = 1;
#$out_dir = "_build";
# useful for git version number generation
$go_mode = 1;
# tell pdflatex not to insert hard line breaks
$ENV{max_print_line} = $log_wrap = 1000;
$ENV{TEXMFHOME} = "./texmf";
$pdflatex = '(echo "\\\\\\\\newcommand{\\\\\\\\version}{"`(git describe --always --long --dirty 2>/dev/null || echo "unknown")`"}") > version.tex; texfot pdflatex -interaction nonstopmode -halt-on-error -file-line-error -synctex=1 %O %S';
$pdf_previewer = 'zathura -x "vim --servername %R --remote +%{line} %{input}" %S & xterm -e vim -c "let g:tex_pdf_output_file=\"%S\"" --servername %R %T';
$pdf_update_mode = 1;
