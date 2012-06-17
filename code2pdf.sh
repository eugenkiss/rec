#!/bin/bash
lhs2tex --tt -o doc/code.tex lhs2tex.tex
latexmk -cd -pdf doc/code.tex
latexmk -cd -c   doc/code.tex
rm doc/code.ptb
