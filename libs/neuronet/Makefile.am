#!/bin/bash

SUBDIRS = 

include version.mk

lib_LTLIBRARIES = libneuronet.la


libneuronet_la_SOURCES = \
			 util.c \
			 slave.c \
			 status.c \
			 epoll.c \
			 master.c \
			 client.c \
			 server.c \
			 interface.c \
			 packet.c
	     
libneuronet_la_LDFLAGS = \
		@NEUROLDFLAGS@ \
		-version-info $(INTERFACE):$(REVISION):$(BINARY)

libneuronet_la_LIBADD =

AM_CFLAGS = @NEUROFLAGS@
