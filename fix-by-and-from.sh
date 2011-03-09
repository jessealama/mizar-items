#!/bin/sh

file=$1;

cat $file \
    | tr '\n' '\0' \
    | /Users/alama/sources/mizar/mizar-items/null.pl \
    | sed -e 's/ from\d0\([^\d0]*[\d0]\)/ from \1/g' -e 's/ by\d0\([^\d0]*[\d0]\)/ by \1/g' \
    | tr '\0' '\n';

exit $?