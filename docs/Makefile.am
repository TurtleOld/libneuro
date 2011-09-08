#!/bin/bash

EXTRA_DIST = neuroman.tcl make_man.sh

manuals = *.3

htmls = *.html

man_MANS = $(manuals)


noinst_SCRIPTS = $(manuals)

$(manuals): neuroman.tcl make_man.sh
	./make_man.sh

clean-local:
	-rm -f $(manuals) $(htmls)

srcdir = @srcdir@
VPATH = @srcdir@
