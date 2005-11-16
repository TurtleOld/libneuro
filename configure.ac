
AC_PREREQ(2.59)

AC_INIT(README)

# versionning technique from Simple DirectMedia Layer (which they took from Gtk apparently)
#
# NEURO_MICRO_VERSION += 1	when bug fixes were done or small changes
# NEURO_MINOR_VERSION += 1	when new functions and or modules were added, anything more than mere bug fixes or small changes
# NEURO_MAJOR_VERSION += 1	when set goals for the whole projects are met, example when neuro will be done, we will raise that by 1. After it is 1, it only raise when most(like more than half) of the code was changes.
# if backward compatibility has been broken, put the 2 below to 0
# NEURO_INTERFACE_AGE += 1	if new functions were added put this to 0, else += 1
# NEURO_BINARY_AGE += 1		anything done adds 1
#
NEURO_MAJOR_VERSION=0
NEURO_MINOR_VERSION=20
NEURO_MICRO_VERSION=10
NEURO_INTERFACE_AGE=0
NEURO_BINARY_AGE=10
NEURO_VERSION=$NEURO_MAJOR_VERSION.$NEURO_MINOR_VERSION.$NEURO_MICRO_VERSION

AC_SUBST(NEURO_MAJOR_VERSION)
AC_SUBST(NEURO_MINOR_VERSION)
AC_SUBST(NEURO_MICRO_VERSION)
AC_SUBST(NEURO_INTERFACE_AGE)
AC_SUBST(NEURO_BINARY_AGE)
AC_SUBST(NEURO_VERSION)


# libtool versioning
LT_RELEASE=$NEURO_MAJOR_VERSION.$NEURO_MINOR_VERSION
LT_CURRENT=`expr $NEURO_MICRO_VERSION - $NEURO_INTERFACE_AGE`
LT_REVISION=$NEURO_INTERFACE_AGE
LT_AGE=`expr $NEURO_BINARY_AGE - $NEURO_INTERFACE_AGE`

AC_SUBST(LT_RELEASE)
AC_SUBST(LT_CURRENT)
AC_SUBST(LT_REVISION)
AC_SUBST(LT_AGE)

#AC_INIT(Neuro, $NEURO_VERSION)
AC_CONFIG_SRCDIR(src/neuro_main.c)

# init automake
AM_INIT_AUTOMAKE(Neuro, $NEURO_VERSION)

# set some variables
AC_PROG_CC
AM_PROG_LIBTOOL

# set the flags
CFLAGS="$CFLAGS -ansi -Wall -pedantic -I\$(top_srcdir)/src" #-std=gnu99
NEUROFLAGS="-D__USE_GNU"
SDLFLAGS="$(sdl-config --cflags)"
#NEUROFLAGS="-std=gnu99 -Wall -pedantic -I/usr/include/libxml2 $(sdl-config --cflags)"
XML2FLAGS="-I/usr/include/libxml2"

#------ configure arguments ------

# need to make it so if it is set to 0, that it doesnt compile xmltool.c and nmap.c
AC_ARG_ENABLE(libxml2, 
[  --enable-libxml2	enable the use of libxml2 for use with the xmltools [[default=yes]]],
	, enable_libxml2=yes)
if test x$enable_libxml2 = xyes; then
	AC_CHECK_LIB(xml2, xmlFreeDoc, [NEUROLIBS="$NEUROLIBS -lxml2" AC_SUBST(XML2FLAGS) LIBS="$LIBS -lxml2"
] , AC_MSG_ERROR([You must have libxml2]))
fi
#AM_CONDITIONAL(USE_XML, test x$enable_libxml2 = xyes)

AC_ARG_WITH(lowlib, 
[ --with-lowlib=<lowlib>     the low library Neuro will use;]
[ 			   only a single value is valid here;]
[			   Default is "sdl";]
[ 			   Possible low libraries are :]
[			     dummy, sdl ],
lowlib="$withval", lowlib="sdl")
SELECTED_LOWLIB=`echo $lowlib`

AM_CONDITIONAL(USE_SDL, test x$SELECTED_LOWLIB = xsdl)
if test $SELECTED_LOWLIB = sdl; then
	AC_CHECK_LIB(SDL, SDL_Init, [AC_DEFINE(USE_SDL, 1) LIBS="$LIBS $(sdl-config --libs)" NEUROFLAGS="$NEUROFLAGS $SDLFLAGS"], [AC_MSG_ERROR(You don't have libSDL, you can get it from http://www.libsdl.org)])

fi


# need to make it so if it is set to 0, it will still compile :D
#AC_ARG_ENABLE(libSDL, 
#[  --enable-libSDL	enable the use of the library Simple DirectMedia Layer [[default=yes]]],
#	, enable_libSDL=yes)
#if test x$enable_libSDL = xyes; then
#	AC_CHECK_LIB(SDL, SDL_Init, [AC_DEFINE(USE_SDL, 1) LIBS="$LIBS $(sdl-config --libs)" NEUROFLAGS="$NEUROFLAGS $SDLFLAGS"
#	], [AC_MSG_ERROR(You don't have libSDL, you can get it from http://www.libsdl.org)])
#else
#	AC_DEFINE(USE_SDL, 0)
#fi
#AM_CONDITIONAL(USE_SDL, test x$enable_libSDL = xyes)

AC_ARG_ENABLE(Debug,
[  --enable-Debug	enable the debugging of this program [[default=no]]],
	, enable_Debug=no)
if test x$enable_Debug = xyes; then
	NEUROFLAGS="$NEUROFLAGS -g"
else
	NEUROFLAGS="$NEUROFLAGS -O3 -s"
fi

AC_SUBST(NEUROFLAGS)

# set the installation path for the headers
includedir=$includedir/Neuro

# enumerate the Makefiles to be created
AC_CONFIG_FILES([
Makefile
src/Makefile
src/xml/Makefile
src/memory/Makefile
src/video/Makefile
src/events/Makefile
src/extlib/Makefile
src/misc/Makefile
src/gui/Makefile
src/script/Makefile
src/sound/Makefile
src/sound/music/Makefile
])
#add to AC_CONFIG_FILES when they are started
#src/gui/Makefile
#src/script/Makefile
#src/sound/Makefile
#src/sound/music/Makefile


#AC_MSG_NOTICE([This is the low library Neuro will use : $SELECTED_LOWLIB])

AC_OUTPUT

echo " "
echo "This is the low library Neuro will use : $SELECTED_LOWLIB"
echo " "
