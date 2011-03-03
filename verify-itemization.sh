#!/bin/bash

article=$1;

article_in_ramdisk=/Volumes/ramdisk/$article;
article_on_harddisk=/tmp/$article;

cp -R $article_on_harddisk $article_in_ramdisk;
cd $article_in_ramdisk;
items=`ls text/ckb*.miz`;
for item in $items; do
    # accom -q -s -l $item > /dev/null 2> /dev/null ;
    # if [[ $? -eq "0" ]]; then
	verifier -q -s -l $item > /dev/null 2>/dev/null;
	if [[ ! $? -eq "0" ]]; then
	    echo "$article:$item: not ok (verifier error)";
	    rm -Rf $article_in_ramdisk;
	    exit 1;
	fi;
	#else
	# echo "$article:$item: not ok (accommodator error)";
	# exit 1;
    # fi;
done
echo "$article: ok";
rm -Rf $article_in_ramdisk;

exit 0;
