#!/bin/bash

noinst_LTLIBRARIES = libextlib.la

AM_CFLAGS= @NEUROFLAGS@

if USE_SDL
lib_sources = sdl.c
#else USE_GGI
#lib_sources = ggi.c
else
lib_sources = none.c
endif

libextlib_la_SOURCES = $(lib_sources) 
