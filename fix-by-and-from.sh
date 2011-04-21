#!/bin/sh

file=$1;

null_script=/Users/alama/sources/mizar/mizar-items/null.pl;

cat $file \
    | tr '\n' '\0' \
    | $null_script \
    | sed -e 's/ from\d0\([^\d0]*[\d0]\)/ from \1/g' -e 's/ by\d0\([^\d0]*[\d0]\)/ by \1/g' -e 's/[\d0]by/ by/g' -e 's/[\d0]from / from/g' \
    | tr '\0' '\n';

exit $?