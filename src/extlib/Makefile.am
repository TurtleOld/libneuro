#!/bin/bash

EXTRA_DIST = alpha.inc

noinst_LTLIBRARIES = libextlib.la

#lib_LTLIBRARIES = libneuro_video_none.drv.la libneuro_video_x11.drv.la libneuro_video_sdl.drv.la

AM_CFLAGS= @NEUROFLAGS@



#libneuro_video_none_drv_la_SOURCES = none.c
#libneuro_video_x11_drv_la_SOURCES = x11.c
#libneuro_video_sdl_drv_la_SOURCES = sdl.c
#
#libneuro_video_none_drv_la_LDFLAGS = @NONE_LDFLAGS@
#libneuro_video_x11_drv_la_LDFLAGS = @X11_LDFLAGS@
#libneuro_video_sdl_drv_la_LDFLAGS = @SDL_LDFLAGS@

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
