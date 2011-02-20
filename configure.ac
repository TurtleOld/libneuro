
AC_PREREQ(2.59)

AC_INIT(README)

#
# NEURO_MICRO_VERSION += 1	when bug fixes were done or small changes
# NEURO_MINOR_VERSION += 1	when new functions and or modules were added, anything more than mere bug fixes or small changes
# NEURO_MAJOR_VERSION += 1	when set goals for the whole projects are met, example when neuro will be done, we will raise that by 1. After it is 1, it only raise when most(like more than half) of the code was changes.
#

. version.mk

NEURO_MAJOR_VERSION=$MAJOR
NEURO_MINOR_VERSION=$MINOR
NEURO_MICRO_VERSION=$MICRO
NEURO_INTERFACE_AGE=$INTERFACE
NEURO_REVISION=$REVISION
NEURO_BINARY_AGE=$BINARY
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
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_SRCDIR([src/neuro_main.c])
AC_CONFIG_HEADERS([include/neuro/config.h])

# init automake
AM_INIT_AUTOMAKE(neuro, $NEURO_VERSION)

# call canonical host to get valuable informations about the current system
AC_CANONICAL_HOST
AC_DEFINE_UNQUOTED(OS, "${host}", [cpu-machine-OS])
AC_MSG_RESULT($host)

#dnl ----- set some variables ------

# check for programs

# ANSI C compiler check
AC_PROG_CC

# C preprocessor check
AC_PROG_CPP
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET

# check for cygwin stuff
AC_LIBTOOL_WIN32_DLL

# libtool setup
AM_PROG_LIBTOOL

dnl we get rid of the -g default flag
if test $CFLAGS = "-g"; then
	CFLAGS=""
fi

# set the flags
CFLAGS="$CFLAGS -std=iso9899:1990 -Wall -pedantic -I\$(top_srcdir)/src" 
NEUROFLAGS="-D__USE_GNU -I\${top_srcdir}/include -I\${top_srcdir}/include/neuro"
NEUROLDFLAGS="-export-symbols \$(top_srcdir)/include/libneuro.sym"
SDLFLAGS="`sdl-config --libs`" 

dnl ------ Configure Arguments ------

AC_ARG_ENABLE(Debug, [  --enable-Debug	enable the debugging of this program [[default=no]]], , enable_Debug=no)

AC_ARG_WITH(lowlib, 
[ --with-lowlib=<lowlib>     the low library Neuro will use;]
[ 			   only a single value is valid here;]
[			   Default is "sdl";]
[ 			   Possible low libraries are :]
[			     dummy, X11, sdl ],
lowlib="$withval", lowlib="nothing")

AC_ARG_ENABLE(video, [  --disable-video	disables all the video particularities of this library [[default=no]]], enable_Video=no, enable_Video=yes)

enable_Neuronet=no

AC_ARG_ENABLE(neuronet, [  --disable-neuronet disables the neuronet library from being built [[default=no]]], enable_Neuronet=no, enable_Neuronet=yes)

AC_ARG_WITH(pollType, 
[  --with-pollType=<pollType>	which of select, poll or epoll]
[	 			   neuronet will use.]
[				   Default is "epoll" under unix systems]
[				   and "select" under win32 (no other choice under win32)]
[ 				   Possible valid choices are :]
[				     epoll, poll, select ],
pollType="$withval", pollType="nothing")

freetype_enable=no
zlib_enable=no

# host related checkings
AC_MSG_CHECKING([if we need -no-undefined])
case $host in
	*-*-cygwin | *-*-mingw* | *-*-pw32*)
		NEUROFLAGS="$NEUROFLAGS -DWIN32"
		NEUROLDFLAGS="$NEUROLIBFLAGS -no-undefined"
		need_no_undefined=yes
		targ=w32
		#we disable neuronet no matter what	

		#LIBS="$LIBS -lc -lgcc_s -ldl"
		if test x$lowlib = xnothing; then
			lowlib=sdl
		fi
		;;
	*)
#by default we now compile against X11 on POSIX machines
		if test x$lowlib = xnothing; then
			lowlib=X11
		fi
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
		AC_FUNC_MALLOC
		AC_FUNC_REALLOC
		AC_FUNC_VPRINTF
		AC_CHECK_HEADERS([unistd.h], , [AC_MSG_ERROR([the required header unistd is missing.])])
		AC_CHECK_FUNCS([gettimeofday], , [AC_MSG_ERROR([a required function is missing.])])
	;;
esac

dnl --------- Checks -------------

dnl ----------------
dnl check for header files.
dnl ----------------
AC_PATH_X
AC_HEADER_STDC
AC_CHECK_HEADERS([fcntl.h stdlib.h string.h sys/time.h], , [AC_MSG_ERROR([a required header is missing.])])

# Checks for typedefs, structures, and compiler characteristics.

AC_C_CONST
AC_TYPE_SIZE_T
AC_HEADER_TIME

# Checks for library functions.
AC_FUNC_VPRINTF
AC_CHECK_FUNCS([memchr memset strchr], , [AC_MSG_ERROR([a required function is missing.])])

AC_CHECK_FUNCS([vasprintf])

dnl ----------------
dnl List of libraries Neuro can use
dnl ----------------
SELECTED_LOWLIB=`echo $lowlib`

dnl ----------------
dnl Video check
dnl ----------------
AM_CONDITIONAL(USE_VIDEO, test x$enable_Video = xyes)
if test $enable_Video = no; then
# we disable the low library by putting it to the dummy driver
	SELECTED_LOWLIB=dummy
else
	AC_DEFINE(USE_VIDEO, 1, ["Video is activated"])
fi

dnl ----------------
dnl SDL check
dnl ----------------
AM_CONDITIONAL(USE_SDL, test x$SELECTED_LOWLIB = xsdl)
if test $SELECTED_LOWLIB = sdl; then
	if test $targ = unix; then
		AC_CHECK_LIB(SDL, SDL_Init, 
		     [AC_DEFINE(USE_SDL, 1, ["SDL main flag"]) LIBS="$LIBS $SDLFLAGS"
		      NEUROFLAGS="$NEUROFLAGS `sdl-config --cflags`"], 
		     [AC_MSG_ERROR(You don't have libSDL you can get it from http://www.libsdl.org)
])

		
	else
		AC_CHECK_LIB(SDL, SDL_Init, 
		     [AC_DEFINE(USE_SDL, 1, ["SDL main flag"]) LIBS="$LIBS -lSDL"], 
		     [AC_MSG_ERROR(You don't have libSDL you can get it from http://www.libsdl.org)
])

		
	fi

	freetype_enable=yes
	zlib_enable=yes
fi

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
		     [AC_DEFINE(USE_X11, 1, ["X11 main flag"]) LIBS="$LIBS -lX11 $X_PRE_LIBS $X_LIBS $X_EXTRA_LIBS"], 
		     [AC_MSG_ERROR(You don't have libX11)])
#	LIBS="$LIBS $X_PRE_LIBS $X_LIBS $X_EXTRA_LIBS"
#	NEUROFLAGS="$NEUROFLAGS $X_CFLAGS"

	freetype_enable=yes
	zlib_enable=yes
fi

dnl ----------------
dnl Zlib check
dnl ----------------
if test x$zlib_enable = xyes; then
	if test x$targ = xw32; then
		AC_CHECK_LIB(zdll, gzgetc,
		     [AC_DEFINE(USE_ZLIB, , ["ZLIB main flag"])] LIBS="$LIBS -lzdll",
		     [AC_MSG_ERROR(You don't have the Z library)])
	else
		AC_CHECK_LIB(z, gzgetc, 
		     [AC_DEFINE(USE_ZLIB, , ["ZLIB main flag"])] LIBS="$LIBS -lz", 
		     [AC_MSG_ERROR(You don't have the Z library)])
	fi
fi




dnl ----------------
dnl libmath check
dnl ----------------
AC_CHECK_LIB(m, log10, , [AC_MSG_ERROR(You don't have libm -- math)])


dnl ----------------
dnl Freetype2 Check
dnl ----------------
AM_CONDITIONAL(USE_FREETYPE2, test x$freetype_enable = xyes)
if test x$freetype_enable = xyes; then
	AC_CHECK_LIB(freetype, FT_New_Face,
		     [LIBS="$LIBS `freetype-config --libs`" CFLAGS="$CFLAGS `freetype-config --cflags`"], 
		     [AC_MSG_ERROR(You don't have the libfreetype2 library you can get it from freetype.sourceforge.net)])
fi

dnl ----------------
dnl Neuronet checks
dnl ----------------

if test x$enable_Neuronet = xyes; then

dnl ----------------
dnl libwsock32 check
dnl ----------------
if test x$targ = xw32; then
#AC_CHECK_LIB(wsock32, WSAStartup, 
#	     , [AC_MSG_ERROR(you don't have libwsock32)])
	LIBS="$LIBS -lwsock32"
fi

if test x$pollType = xepoll; then
	AC_CHECK_HEADER(sys/epoll.h, epoll_header=yes, epoll_header=no)

	AC_MSG_CHECKING([epoll functions implemented and useable])
	if test x$epoll_header = xyes; then
		AC_RUN_IFELSE(
			[AC_LANG_SOURCE([[
		#include <stdlib.h> /* exit */
		#include <unistd.h> /* close */
		#include <sys/epoll.h>

		int main()
		{
			int _err = 0;

			_err = epoll_create(1);

			if (_err == -1)
				exit(1);

			close(_err);

			exit(0);
		}
			]])], [epoll_success=yes] , [epoll_success=no])

		if test x$epoll_success = xyes; then
			AC_MSG_RESULT(yes)
			AC_DEFINE(HAVE_EPOLL, 1, The epoll function)
		else
			AC_MSG_RESULT(no)
			AC_MSG_WARN([The epoll functions do not work, going with poll]) 
			pollType=poll
		fi
	else
		AC_MSG_WARN([The epoll functions do not work, going with poll]) 
		pollType=poll
	fi
fi

if test x$pollType = xpoll; then
	AC_MSG_NOTICE([The poll function is not yet implemented, going with select])
	pollType=select
fi

if test x$pollType = xselect; then
	if test x$targ == xw32; then
		# the headers under w32 are different, so we won't even bother
		# checking for the availability.
		AC_DEFINE(HAVE_SELECT, 1, The select function)
	else
		AC_CHECK_HEADER(sys/select.h, [AC_DEFINE(HAVE_SELECT, 1, The select function)], [AC_MSG_ERROR([You need the header and library for the select function!])])
	fi
fi

fi # neuronet enabled

dnl ----------------
dnl Debug toggle
dnl ----------------
if test x$enable_Debug = xyes; then
	#NEUROFLAGS="$NEUROFLAGS -g"
	CFLAGS="$CFLAGS -g"
else
	#NEUROFLAGS="$NEUROFLAGS -O2 -s"
	CFLAGS="$CFLAGS -Os -s"
fi

AC_SUBST(NEUROFLAGS)
AC_SUBST(NEUROLDFLAGS)

AM_CONDITIONAL(USE_NEURONET, test x$enable_Neuronet = xyes)

# set the installation path for the headers
includedir=$includedir/neuro

echo 

# enumerate the Makefiles to be created
AC_CONFIG_FILES([
Makefile
include/Makefile
include/neuro/Makefile
include/neuro/nnet/Makefile
src/Makefile
src/memory/Makefile
src/video/Makefile
src/events/Makefile
src/extlib/Makefile
src/misc/Makefile
src/misc/bitmap/Makefile
docs/Makefile
libs/Makefile
libs/neuronet/Makefile
])

AC_OUTPUT

echo " "
echo "enable neuronet : $enable_Neuronet"
echo "This is the low library Neuro will use : $SELECTED_LOWLIB"
echo " "
