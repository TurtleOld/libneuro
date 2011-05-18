#!/bin/bash

noinst_HEADERS = \
		client.h \
		common.h \
		epoll.h \
		master.h \
		server.h \
		slave.h \
		status.h \
		util.h

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
		@NNETLDFLAGS@ \
		-version-info $(INTERFACE):$(REVISION):$(BINARY)

libneuronet_la_LIBADD =

AM_CFLAGS = @NNETFLAGS@
