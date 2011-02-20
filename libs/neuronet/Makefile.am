#!/bin/bash

SUBDIRS = 

include version.mk

lib_LTLIBRARIES = libneuronet.la


libneuronet_la_SOURCES = \
		     netabs.c \
		     network.c
	     
libneuronet_la_LDFLAGS = \
		@NEUROLDFLAGS@ \
		-version-info $(INTERFACE):$(REVISION):$(BINARY)

libneuronet_la_LIBADD =

AM_CFLAGS = @NEUROFLAGS@
