#! /bin/bash

vpathEntry="srcdir = @srcdir@\nVPATH = @srcdir@"

elemCont="^[ \t]\+.*\\\\"
elemEnd="^[ #\t]\+.*"

outputRest=": r; {N; b r}"

globSubdirsTail=": t; /$elemEnd/ {n; b t}; {a\
\\\\n$vpathEntry
; $outputRest}"

#cat $1 | sed -e "/SUBDIRS/,$ {/$elemCont/! {/$elemEnd/ {$globSubdirsTail}}}" | sed -e "/\(srcdir\|VPATH\)/ {$outputRest}; {$ a\
#$vpathEntry
#}"

if [ `cat $1 | sed -n -e "/\(srcdir\|VPATH\)/ {s/.*/1/p; q}; $ s/.*/0/p"` == "0" ]; then
	sed -i -e "/SUBDIRS/,$ {/$elemCont/! {/$elemEnd/ {$globSubdirsTail}}}" $1
	sed -i -e "/\(srcdir\|VPATH\)/ {$outputRest}; {$ a\
	\\\\n$vpathEntry
	}" $1

fi

