#!/bin/bash

noinst_LTLIBRARIES = libmisc.la

lib_sources = 	\
		other.c \
		chgen.c \
		arg.c

if USE_X11
	lib_sources += bitmap.c
endif

libmisc_la_SOURCES = $(lib_sources)

AM_CFLAGS= @NEUROFLAGS@
#AM_CFLAGS= @SDLFLAGS@

#include_HEADERS= \
#		     other.h \
#		     typedef.h

