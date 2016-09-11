openemacs
===

openemacs is a tiny emacs clone without any library dependencies.

The goal of the project is to implement the most important parts of emacs while staying under 1024 lines of code (as counted by `cloc`).

Usage:

    openemacs <filename>

Keys:

    ctrl-a = Go to start of line
    ctrl-e = Go to end of line
    ctrl-k = Cut/kill line
    ctrl-l = Scroll current line to center and refresh
    ctrl-n = Go to next line (alternative: arrow down)
    ctrl-p = Go to previous line (alternative: arrow up)
    ctrl-q = Quoted insert - insert character as-is
    ctrl-s = Search
    ctrl-y = Yank line
    ctrl-z = Suspend

    ctrl-x + ctrl-s = Save
    ctrl-x + ctrl-c = Exit

Syntax highlighting support:

* C (`*.c`, `*.h`)
* C++ (`*.cpp`, `*.hpp`)
* Python (`*.py`)

openemacs is based on Kilo - a minimal editor written by Salvatore Sanfilippo (antirez).

The code is released under the 2-clause BSD license.
