#!/bin/bash

SUBDIRS =

if USE_VIDEO
SUBDIRS += bitmap
endif

noinst_LTLIBRARIES = libmisc.la

AM_CFLAGS= @NEUROFLAGS@

libmisc_la_SOURCES = \
		other.c \
		arg.c 

srcdir = @srcdir@
VPATH = @srcdir@

if USE_VIDEO
libmisc_la_LIBADD = \
		    bitmap/libbitmap.la
endif
