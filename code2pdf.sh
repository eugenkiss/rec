#!/bin/bash
lhs2tex --tt -o code.tex lhs2tex.tex
latexmk -cd -pdf code.tex
latexmk -cd -c   code.tex
rm code.ptb
