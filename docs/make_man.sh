#!/bin/bash

tmpfile=./.__tmp___-

. ../version.mk

echo 3 > $tmpfile
echo $(date "+%d %b %Y") >> $tmpfile
echo "version ${MAJOR}.${MINOR}.${MICRO}" >> $tmpfile
echo "Libneuro Functions" >> $tmpfile

argument=$(echo ../include/neuro/*.h)

time tclsh ./neuroman.tcl -c $tmpfile -i $argument

time ./neuroman $argument

rm $tmpfile
