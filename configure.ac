
AC_PREREQ(2.59)

AC_INIT(README)

#
# NEURO_MICRO_VERSION += 1	when bug fixes were done or small changes
# NEURO_MINOR_VERSION += 1	when new functions and or modules were added, anything more than mere bug fixes or small changes
# NEURO_MAJOR_VERSION += 1	when set goals for the whole projects are met, example when neuro will be done, we will raise that by 1. After it is 1, it only raise when most(like more than half) of the code was changes.
#
NEURO_MAJOR_VERSION=0
NEURO_MINOR_VERSION=46
NEURO_MICRO_VERSION=0
NEURO_INTERFACE_AGE=1
NEURO_REVISION=0
NEURO_BINARY_AGE=0
NEURO_VERSION=$NEURO_MAJOR_VERSION.$NEURO_MINOR_VERSION.$NEURO_MICRO_VERSION

AC_SUBST(NEURO_MAJOR_VERSION)
AC_SUBST(NEURO_MINOR_VERSION)
AC_SUBST(NEURO_MICRO_VERSION)
AC_SUBST(NEURO_INTERFACE_AGE)
AC_SUBST(NEURO_BINARY_AGE)
AC_SUBST(NEURO_VERSION)


# libtool versioning
LT_CURRENT=$NEURO_INTERFACE_AGE
LT_REVISION=$NEURO_REVISION
LT_AGE=$NEURO_BINARY_AGE

AC_SUBST(LT_CURRENT)
AC_SUBST(LT_REVISION)
AC_SUBST(LT_AGE)

#AC_INIT(Neuro, $NEURO_VERSION)
AC_CONFIG_SRCDIR([src/neuro_main.c])

# init automake
AM_INIT_AUTOMAKE(neuro, $NEURO_VERSION)

# call canonical host to get valuable informations about the current system
AC_CANONICAL_HOST
AC_DEFINE_UNQUOTED(OS, "${host}", [cpu-machine-OS])
AC_MSG_RESULT($host)

#dnl ----- set some variables ------

# check for programs
AC_PROG_CXX
AC_PROG_CC
AC_PROG_CPP
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET

# check for cygwin stuff
AC_LIBTOOL_WIN32_DLL

# libtool setup
AM_PROG_LIBTOOL


# set the flags
CFLAGS="$CFLAGS -ansi -Wall -pedantic -I\$(top_srcdir)/src" 
NEUROFLAGS="-D__USE_GNU -I\${top_srcdir}/include -I\${top_srcdir}/include/neuro"
NEUROLIBFLAGS="--enable-realtime-pseudo-reloc"
SDLFLAGS="`sdl-config --libs`" 

dnl ------ Configure Arguments ------

AC_ARG_ENABLE(libxml2, [  --enable-libxml2	enable the use of libxml2 for use with the xmltools [[default=yes]]], , enable_libxml2=yes)

AC_ARG_ENABLE(Debug, [  --enable-Debug	enable the debugging of this program [[default=no]]], , enable_Debug=no)

AC_ARG_WITH(lowlib, 
[ --with-lowlib=<lowlib>     the low library Neuro will use;]
[ 			   only a single value is valid here;]
[			   Default is "sdl";]
[ 			   Possible low libraries are :]
[			     dummy, X11, sdl ],
lowlib="$withval", lowlib="sdl")


# host related checkings

AC_MSG_CHECKING([if we need -no-undefined])
case $host in
	*-*-cygwin | *-*-mingw* | *-*-pw32*)
		NEUROFLAGS="$NEUROFLAGS -DWIN32"
		NEUROLIBFLAGS="$NEUROLIBFLAGS -no-undefined"
		need_no_undefined=yes
		targ=w32
		#LIBS="$LIBS -lc -lgcc_s -ldl"
		;;
	*)
		targ=unix
		need_no_undefined=no
		;;
esac
AC_MSG_RESULT($need_no_undefined)
AM_CONDITIONAL(NO_UNDEFINED, test x$need_no_undefined = xyes)

case $targ in
	w32)
		AC_CHECK_HEADERS([windows.h], , [AC_MSG_ERROR([the required header windows is missing.])])
		#AC_CHECK_FUNCS([GetTickCount], , [AC_MSG_ERROR([a required function is missing.])])
	

	;;

	unix)
		AC_FUNC_REALLOC
		AC_CHECK_HEADERS([unistd.h], , [AC_MSG_ERROR([the required header unistd is missing.])])
		AC_CHECK_FUNCS([gettimeofday], , [AC_MSG_ERROR([a required function is missing.])])
	;;
esac

dnl --------- Checks -------------

dnl ----------------
dnl check for header files.
dnl ----------------
AC_HEADER_STDC
AC_CHECK_HEADERS([fcntl.h stdlib.h string.h sys/time.h], , [AC_MSG_ERROR([a required header is missing.])])

# Checks for typedefs, structures, and compiler characteristics.

AC_C_CONST
AC_TYPE_SIZE_T
AC_HEADER_TIME

# Checks for library functions.
AC_FUNC_VPRINTF
AC_CHECK_FUNCS([memchr memset strchr], , [AC_MSG_ERROR([a required function is missing.])])

dnl ----------------
dnl XML check
dnl ----------------
if test x$enable_libxml2 = xyes; then
  AC_CHECK_LIB(xml2, 
		 xmlFreeDoc, 
		 [NEUROFLAGS="$NEUROFLAGS -I/usr/include/libxml2 -I/usr/local/include/libxml2" NEUROLIBS="$NEUROLIBS -lxml2" LIBS="$LIBS -lxml2"] , 
		 AC_MSG_ERROR([You must have libxml2])
		 )
fi
AM_CONDITIONAL(USE_XML, test x$enable_libxml2 = xyes)

dnl ----------------
dnl List of libraries Neuro can use
dnl ----------------
SELECTED_LOWLIB=`echo $lowlib`


dnl ----------------
dnl SDL check
dnl ----------------
AM_CONDITIONAL(USE_SDL, test x$SELECTED_LOWLIB = xsdl)
if test $SELECTED_LOWLIB = sdl; then
	if test $targ = unix; then
		AC_CHECK_LIB(SDL, SDL_Init, 
		     [AC_DEFINE(USE_SDL, 1) LIBS="$LIBS $SDLFLAGS"
		      NEUROFLAGS="$NEUROFLAGS `sdl-config --cflags`"], 
		     [AC_MSG_ERROR(You don't have libSDL you can get it from http://www.libsdl.org)
])

		
	else
		AC_CHECK_LIB(SDL, SDL_Init, 
		     [AC_DEFINE(USE_SDL, 1) LIBS="$LIBS -lSDL"], 
		     [AC_MSG_ERROR(You don't have libSDL you can get it from http://www.libsdl.org)
])

		
	fi

	AC_CHECK_LIB(SDL_ttf, TTF_RenderUNICODE_Solid, ,
		     [AC_MSG_ERROR(You don't have libSDL_ttf you can get it from http://libsdl.org)])

	cp include/extlib.h include/neuro
	cat include/extlib_sdl.h >> include/neuro/extlib.h
fi
dnl ----------------
dnl Zlib check
dnl ----------------
AC_CHECK_LIB(z, gzgetc, 
	     [AC_DEFINE(USE_ZLIB, 1) LIBS="$LIBS -lz"], 
	     [AC_MSG_ERROR(You don't have the Z library)])


dnl ----------------
dnl X11 check
dnl ----------------
AM_CONDITIONAL(USE_X11, test x$SELECTED_LOWLIB = xX11)
if test $SELECTED_LOWLIB = X11; then
	AC_PATH_X
	AC_PATH_XTRA
	LDFLAGS="$CFLAGS $X_LIBS"
	CFLAGS="$CFLAGS $X_CFLAGS"
	AC_CHECK_LIB(X11, XCreateGC, 
		     [AC_DEFINE(USE_X11, 1) LIBS="$LIBS $X_PRE_LIBS $X_LIBS $X_EXTRA_LIBS"], 
		     [AC_MSG_ERROR(You don't have libX11)])
#	LIBS="$LIBS $X_PRE_LIBS $X_LIBS $X_EXTRA_LIBS"
#	NEUROFLAGS="$NEUROFLAGS $X_CFLAGS"
	AC_CHECK_LIB(Xpm, XpmCreatePixmapFromData,
		     [LIBS="$LIBS -lXpm"],
		     [AC_MSG_ERROR(You don't have libXpm)])
	cp include/extlib.h include/neuro
fi


dnl ----------------
dnl Debug toggle
dnl ----------------
if test x$enable_Debug = xyes; then
	NEUROFLAGS="$NEUROFLAGS -g"
else
	NEUROFLAGS="$NEUROFLAGS -O2 -s"
fi

AC_SUBST(NEUROFLAGS)
AC_SUBST(NEUROLIBFLAGS)

# set the installation path for the headers
includedir=$includedir/neuro

echo 

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
])
#add those above when they are started
#src/gui/Makefile
#src/script/Makefile
#src/sound/Makefile
#src/sound/music/Makefile



#AC_MSG_NOTICE([This is the low library Neuro will use : $SELECTED_LOWLIB])

AC_OUTPUT

echo " "
echo "This is the low library Neuro will use : $SELECTED_LOWLIB"
echo " "
