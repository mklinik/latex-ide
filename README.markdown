# latex-ide

Have one command that spawns a pdf viewer and an editor and automatically
compiles the tex file when it changes.

latex-ide is deprecated. I found out that latexmk can do everything I want.
Just use the latexmkrc in this repository.

- latexmk supports out-of-source builds
- latexmk -pvc supports watching for file changes and automatically runs pdflatex
- by specifying two programs as `$pdf_previewer`, latexmk spawns the pdf viewer
  and the editor. I set it up so that they can talk to each other via synctex.
- multiple instances for different tex files are supported by using vim's
  --servername feature.
- mklatex knows how to find .bst and .bib files in subdirectories
- mklatex -pvc monitors the .bib file, something latex-ide couldn't do

The only thing latexmk doesn't do is error filtering. You always get the whole
barrage of pdflatex output. There seem to be tools for that, but I haven't
looked into that now. latex-ide's error filtering was not very precise anyway.


## Usage

- Have a tex distribution with latexmk installed. I use texlive on debian. It
  installs latexmk by default.
- Have zathura and gvim installed
- Copy the file `latexmkrc` to your home directory or to where your .tex file sits.

    $ latexmk -pvc test.tex


## Synctex

- Ctrl-click on the pdf to jump to the source location in vim.
- For jumping to the pdf location from the source code, have the following in your vimrc:

    function! SyncTexForward()
      let execstr = "silent !zathura --synctex-forward " . line(".") . ":0:% _build/%:r.pdf"
      exec execstr
    endfunction
    nmap <Leader>f :call SyncTexForward()<CR><C-l>


## TODO

- implement the git version feature: When the tex file is in a git repository,
  generate a version.tex file before running pdflatex


## Deprecated

The old sources for latex-ide can be found on the branch `deprecated`.
