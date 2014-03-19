make-latex
----------

* Automatically runs latex when the source file changes.
* Re-runs once if necessary.
* Uses inotify.
* Minimal error handling.

Usage
-----

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

Written in Haskell because Haskell is the better scripting language.
