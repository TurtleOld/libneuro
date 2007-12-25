#!/bin/bash

SUBDIRS = \
	  memory \
	  misc \
	  extlib \
	  events

if USE_VIDEO
SUBDIRS += video
endif

lib_LTLIBRARIES = libneuro.la

libneuro_la_SOURCES = \
		      debug.c \
		      neuro_main.c


libneuro_la_LDFLAGS = \
		      @NEUROLDFLAGS@ \
		      -version-info $(LT_CURRENT):$(LT_REVISION):$(LT_AGE)


SUBLIBS = \
	     memory/libmemory.la \
	     misc/libmisc.la \
	     extlib/libextlib.la \
	     events/libevents.la 

if USE_VIDEO
SUBLIBS += video/libvideo.la
endif

libneuro_la_LIBADD = $(SUBLIBS)
