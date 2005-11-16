#!/bin/bash

SUBDIRS = \
	  memory \
	  misc \
	  extlib \
	  video \
	  events \
	  script \
	  sound \
	  gui \
	  xml

lib_LTLIBRARIES = libneuro.la

lib_sources = 	\
		debug.c \
		neuro_main.c

libneuro_la_SOURCES = $(lib_sources)

AM_CFLAGS= @NEUROFLAGS@
#AM_CFLAGS= @SDLFLAGS@

libneuro_la_LDFLAGS = \
		      -no-undefines \
		      -release $(LT_RELEASE) \
		      -version-info $(LT_CURRENT):$(LT_REVISION):$(LT_AGE)

libneuro_la_LIBADD = \
		     memory/libmemory.la \
		     misc/libmisc.la \
		     extlib/libextlib.la \
		     video/libvideo.la \
		     events/libevents.la \
		     script/libscript.la \
		     sound/libsound.la \
		     gui/libgui.la \
		     xml/libxmltool.la
		     

include_HEADERS= \
		     debug.h \
		     NEURO.h \
		     engine.h
