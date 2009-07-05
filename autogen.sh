#!/bin/bash

autoheader

aclocal

libtoolize

automake --add-missing

libtoolize

autoreconf -v

echo "	You can now run ./configure"
echo "	Then compile by doing : make"
echo "	And then install by doing (as root or otherwise) : make install"
