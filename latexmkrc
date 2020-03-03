$pdf_mode = 1;
#$out_dir = "_build";
# useful for git version number generation
$go_mode = 1;
# tell pdflatex not to insert hard line breaks
$ENV{max_print_line} = $log_wrap = 1000;
$ENV{TEXMFHOME} = "./texmf";
$pdflatex = '(./mkVersionTex.sh || echo "version script not present"); pdflatex -interaction nonstopmode -halt-on-error -file-line-error -synctex=1 %O %S';
$pdf_previewer = 'zathura -x "vim --servername `md5sum %T | proj 1 ` --remote +%{line} %{input}" %S & tvim -c "let g:tex_pdf_output_file=\"%S\"" --servername `md5sum %T | proj 1` %T';
$pdf_update_mode = 1;
