# -*- makefile -*-
PDFLATEX  = pdflatex
EMACS     = emacs
CC        = gcc
CFLAGS    = -std=c11 -Wall -g                             \
		-D_GNU_SOURCE -DLINUX -D_XOPEN_SOURCE=700 \
		-Wno-sign-compare                         \
		-Wno-unused-parameter

TEMPFILES = *.aux *.log *.out core core.* *.o .\#* temp.*
PDFFILES  = *.pdf

# scheme implementations
RACKET    = racket
GUILE     = guile

# build tools
GUILD     = guild
RACO      = raco
