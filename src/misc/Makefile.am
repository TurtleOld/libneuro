#!/bin/bash

noinst_LTLIBRARIES = libmisc.la

AM_CFLAGS= @NEUROFLAGS@

libmisc_la_SOURCES = 	\
		other.c \
		chgen.c \
		arg.c

if USE_X11
libmisc_la_SOURCES += bitmap.c
endif

