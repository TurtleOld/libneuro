#!/bin/bash

#the engine is compiled into a shared object which is linked with the game binary

include version.mk

NAME=libneuro
VERSION=$(MAJOR).$(MEDIUM).$(MINOR)

AR=ar
RANLIB=ranlib
GCC=gcc

# legend :
# 0 = normal
# 1 = debug
# 2 = release
TYPE=2

# compile the library has a shared or static library
# legend :
# 0 = shared
# 1 = static
TYPE2=0

# make the library use static or shared dependencies 
# (only does something when the lib is linked as a shared object)
# legend :
# 0 = shared
# 1 = static
TYPE3=0

# select which of the graphics library you want to use
# 0 = none
# 1 = Simple Direct Layer (SDL)
TYPE4=1

OBJ_FLAG=
FLAGS= -std=gnu99 -pedantic -Wall -I/usr/include/libxml2 -I/usr/include/sdl_lib `sdl-config --cflags`
# flags that r put at the end of the command
ENDFLAGS=
LIBS= -lc `sdl-config --libs`
DEFINES=
DEBUG= -ggdb3 -fprofile-arcs -ftest-coverage
RELEASE= -s -O9
SLIB= #-lSDL_image
#events.o needs to become an engine
ENGINES=graphics.o events.o debug.o xmltool.o nmap.o other.o
OBJECTS=$(ENGINES)
TARGETS= make-engine

ifeq ($(TYPE), 1)
	FLAGS += $(DEBUG)
	ENDFLAGS += -pg
endif
ifeq ($(TYPE), 2)
	FLAGS += $(RELEASE)
endif


ifeq ($(TYPE2), 0)
	OBJ_FLAG += -fPIC
	RNAME = $(NAME).so
endif
ifeq ($(TYPE2), 1)
	RNAME = $(NAME).a
endif

ifeq ($(TYPE3), 0)
	LIBS += -lxml2
endif
ifeq ($(TYPE3), 1)
	LIBS += /usr/lib/libxml2.a
endif

ifeq ($(TYPE4), 1)
	DEFINES += -D USE_SDL
endif


all: $(TARGETS)
	
make-engine: $(RNAME)

$(RNAME): $(OBJECTS)
ifeq ($(TYPE2), 0)
	$(GCC) -shared $(FLAGS) -D VERSION=\"$(VERSION)\" $(DEFINES) -o $(RNAME) $(OBJECTS) $(LIBS) $(SLIB) $(ENDFLAGS)
endif
ifeq ($(TYPE2), 1)
	$(AR) rc $(RNAME) $(OBJECTS)
	$(RANLIB) $(RNAME)
endif

%.o: %.c
	$(GCC) $(FLAGS) -D VERSION=\"$(VERSION)\" $(DEFINES) $(OBJ_FLAG) -c $< $(ENDFLAGS)

clean: 
	rm -f $(RNAME) *.o *.da *.bbg *.bb *.out

mrproper: clean
	rm -f *~
