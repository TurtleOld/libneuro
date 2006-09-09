#!/bin/bash

SUBDIRS = \
	  memory \
	  misc \
	  extlib \
	  video \
	  events


#	  script 
#	  sound 
#	  gui 

lib_LTLIBRARIES = libneuro.la

lib_sources = 	\
		debug.c \
		neuro_main.c

libneuro_la_SOURCES = $(lib_sources)

AM_CFLAGS= @NEUROFLAGS@
#AM_CFLAGS= @SDLFLAGS@

LIB_FLAGS= @NEUROLIBFLAGS@

LIB_FLAGS += \
	    -version-info $(LT_CURRENT):$(LT_REVISION):$(LT_AGE)
	     
# -release $(LT_RELEASE)
	     	      
libneuro_la_LDFLAGS = $(LIB_FLAGS)


SUBLIBLIST= \
	     memory/libmemory.la \
	     misc/libmisc.la \
	     extlib/libextlib.la \
	     video/libvideo.la \
	     events/libevents.la 

#		     script/libscript.la 
#		     sound/libsound.la 
#		     gui/libgui.la 

libneuro_la_LIBADD = $(SUBLIBLIST)

