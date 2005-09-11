#!/bin/bash

SUBDIRS = xml

lib_LTLIBRARIES = libneuro.la

lib_sources = 	debug.c \
		events.c \
		graphics.c \
		other.c

if USE_SDL
lib_lal = sdl.c
else
lib_lal = none.c
endif

libneuro_la_SOURCES = $(lib_sources) $(lib_lal)

AM_CFLAGS= @NEUROFLAGS@
#AM_CFLAGS= @SDLFLAGS@

libneuro_la_LDFLAGS = \
		      -no-undefines \
		      -release $(LT_RELEASE) \
		      -version-info $(LT_CURRENT):$(LT_REVISION):$(LT_AGE)

libneuro_la_LIBADD = \
		     xml/libxmltool.la

include_HEADERS= \
		     debug.h \
		     extlib.h \
		     events.h \
		     graphics.h \
		     other.h \
		     xml/xmltool.h \
		     xml/nmap.h \
		     engine.h \
		     typedef.h \
		     NEURO.h 
