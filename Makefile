#!/bin/bash

#the engine is compiled into a shared object which is linked with the game binary

NAME=libneuro
VERSION=0.8.1

AR=ar
RANLIB=ranlib
GCC=gcc

# legend :
# 0 = normal
# 1 = debug
# 2 = release
TYPE=1

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

OBJ_FLAG=
FLAGS= -std=gnu99 -pedantic -Wall -I/usr/include/libxml2 -I/usr/include/sdl_lib `sdl-config --cflags`
# flags that r put at the end of the command
ENDFLAGS=
LIBS= -lc `sdl-config --libs`
DEFINES=
DEBUG= -g
RELEASE= -s -O3
SLIB= #-lSDL_image
#events.o needs to become an engine
ENGINES=graphics.o events.o debug.o xmltool.o nmap.o other.o
OBJECTS=$(ENGINES)
TARGETS= make-engine

ifeq ($(TYPE), 1)
	FLAGS += -g -fprofile-arcs -ftest-coverage
	ENDFLAGS += -pg 
endif
ifeq ($(TYPE), 2)
	FLAGS += -s -O3
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

mrproper:
