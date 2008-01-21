#!/bin/bash

SUBDIRS = bitmap

noinst_LTLIBRARIES = libmisc.la

AM_CFLAGS= @NEUROFLAGS@

libmisc_la_SOURCES = \
		other.c \
		arg.c 
# 		bitmap.c

libmisc_la_LIBADD = \
		    bitmap/libbitmap.la
