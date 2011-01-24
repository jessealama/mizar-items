#!/bin/bash

article=$1;

cd $article;
items=`ls text/*.miz`;
for item in $items; do
    accom -q -s -l $item > /dev/null 2> /dev/null ;
    if [[ $? -eq "0" ]]; then
	verifier -q -s -l $item > /dev/null 2>/dev/null;
	if [[ $? -eq "0" ]]; then
	    echo "$item: ok";
	else
	    echo "$item: not ok";
	fi;
    else
	echo "$item: not ok";
    fi;
done
cd ..;

exit 0;
