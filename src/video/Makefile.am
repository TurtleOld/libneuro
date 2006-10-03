#!/bin/bash

noinst_LTLIBRARIES = libvideo.la

lib_sources = 	\
		painter.c \
		coredraw.c \
		graphics.c \
		debug.c \
		interface.c 

libvideo_la_SOURCES = $(lib_sources)

AM_CFLAGS= @NEUROFLAGS@
#AM_CFLAGS= @SDLFLAGS@

#LIB_FLAGS= @NEUROLIBFLAGS@
#libvideo_la_LDFLAGS = $(LIB_FLAGS)

#include_HEADERS= \
#		     graphics.h
