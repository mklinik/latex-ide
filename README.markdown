make-latex
----------

* Starts pdf viewer and editor connected by synctex
* pdf viewer is zathura, editor is gvim
* Automatically runs latex when the source file changes.
* Re-runs once if necessary.

Usage
=====

```
$ make-latex test.tex
make-latex: watching test.tex; press Enter to terminate
LaTeX Warning: Reference `foobar' on page 1 undefined on input line 5.
LaTeX Warning: There were undefined references.
make-latex: latex run complete
LaTeX Warning: Reference `foobar' on page 1 undefined on input line 5.
LaTeX Warning: There were undefined references.
LaTeX Warning: Label(s) may have changed. Rerun to get cross-references right.
make-latex: latex run complete
make-latex: rerunning
make-latex: latex run complete

make-latex: bye
```

Installation
============

Building from source requires haskell stack to be installed.
https://docs.haskellstack.org/en/stable/README/

```
$ git clone https://github.com/mklinik/latex-ide.git
$ cd latex-ide
$ stack install
```

The executable `latex-ide` is installed in $HOME/.local/bin
