#!/bin/sh

file=$1

cat $file \
    | tr '\n' '\0' \
    | gsed -e 's/ from\d0\([^\d0]*[\d0]\)/ from \1/g' -e 's/ by\d0\([^\d0]*[\d0]\)/ by \1/g' \
    | tr '\0' '\n'
