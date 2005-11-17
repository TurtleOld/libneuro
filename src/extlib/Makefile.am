#!/bin/bash

noinst_LTLIBRARIES = libextlib.la

if USE_SDL
lib_sources = sdl.c
#else USE_GGI
#lib_sources = ggi.c
else
lib_sources = none.c
endif

libextlib_la_SOURCES = $(lib_sources) 

AM_CFLAGS= @NEUROFLAGS@
#AM_CFLAGS= @SDLFLAGS@

include_HEADERS= \
		     extlib.h
