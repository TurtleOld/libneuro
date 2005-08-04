

SUBDIRS = xml

lib_LTLIBRARIES = libneuro.la

libneuro_la_SOURCES = debug.c \
		     events.c \
		     graphics.c \
		     other.c \
		     sdl.c

#AM_CFLAGS= @NEUROFLAGS@
AM_CFLAGS= @SDLFLAGS@

libneuro_la_LDFLAGS = \
		      -no-undefines \
		      -release $(LT_RELEASE) \
		      -version-info $(LT_CURRENT):$(LT_REVISION):$(LT_AGE)

libneuro_la_LIBADD = \
		     xml/libxmltool.la

include_HEADERS= \
		     debug.h \
		     events.h \
		     graphics.h \
		     other.h \
		     sdl.h \
		     xml/xmltool.h \
		     xml/nmap.h \
		     engine.h \
		     typedef.h \
		     NEURO.h 
