#!/bin/bash

noinst_LTLIBRARIES = libextlib.la

AM_CFLAGS= @NEUROFLAGS@

if USE_SDL
lib_sources = sdl.c
else

if USE_X11
lib_sources = x11.c
else
lib_sources = none.c
endif

endif

libextlib_la_SOURCES = $(lib_sources) 
