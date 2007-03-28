#!/bin/bash

SUBDIRS = \
	  memory \
	  misc \
	  extlib \
	  video \
	  events


lib_LTLIBRARIES = libneuro.la

libneuro_la_SOURCES = \
		      debug.c \
		      neuro_main.c


libneuro_la_LDFLAGS = \
		      @NEUROLDFLAGS@ \
		      -version-info $(LT_CURRENT):$(LT_REVISION):$(LT_AGE)


libneuro_la_LIBADD = \
	     memory/libmemory.la \
	     misc/libmisc.la \
	     extlib/libextlib.la \
	     video/libvideo.la \
	     events/libevents.la 
