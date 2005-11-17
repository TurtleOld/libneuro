#!/bin/bash

noinst_LTLIBRARIES = libevents.la

lib_sources = 	\
		events.c

libevents_la_SOURCES = $(lib_sources)

AM_CFLAGS= @NEUROFLAGS@
#AM_CFLAGS= @SDLFLAGS@

#LIB_FLAGS= @NEUROLIBFLAGS@
#libevents_la_LDFLAGS = $(LIB_FLAGS)

include_HEADERS= \
		     events.h
