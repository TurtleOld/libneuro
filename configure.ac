
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
AM_INIT_AUTOMAKE(neuro, $NEURO_VERSION)

# call canonical host to get valuable informations about the current system
AC_CANONICAL_HOST
AC_DEFINE_UNQUOTED(OS, "${host}", [cpu-machine-OS])
AC_MSG_RESULT($host)

#dnl ----- set some variables ------
# check for programs
AC_PROG_CC

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

dnl ignore this comment because the above now does a good job
dnl --- cheap check for w32 ... --- please make this better, this section should be the
dnl recognition of the platform Neuro is getting built on

#AC_CHECK_FUNC(usleep, , [NEUROFLAGS="$NEUROFLAGS -DWIN32"])



dnl --------- Checks -------------

dnl ----------------
dnl XML check
dnl ----------------
if test x$enable_libxml2 = xyes; then
  AC_CHECK_LIB(xml2, 
		 xmlFreeDoc, 
		 [NEUROLIBS="$NEUROLIBS -lxml2" LIBS="$LIBS -lxml2"] , 
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

	cp include/extlib.h include/neuro
	cat include/extlib_sdl.h >> include/neuro/extlib.h
fi

dnl ----------------
dnl X11 check
dnl ----------------
AM_CONDITIONAL(USE_X11, test x$SELECTED_LOWLIB = xX11)
if test $SELECTED_LOWLIB = X11; then
	AC_PATH_X
	AC_PATH_XTRA
	#AC_CHECK_LIB(X11, XCopyArea, 
	#	     [AC_DEFINE(USE_X11, 1) LIBS="$LIBS -lX11"
	#	      NEUROFLAGS="$NEUROFLAGS -L/usr/X11R6/lib -I/usr/X11R6/include"], 
	#	     [AC_MSG_ERROR(You don't have libX11)])
	LIBS="$LIBS $X_PRE_LIBS $X_LIBS -lXpm $X_EXTRA_LIBS"
	NEUROFLAGS="$NEUROFLAGS $X_CFLAGS"
	#AC_CHECK_LIB(Xpm, XpmCreatePixmapFromData,
	#	     [LIBS="$LIBS -lXpm"],
	#	     [AC_MSG_ERROR(You don't have libXpm)])
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
