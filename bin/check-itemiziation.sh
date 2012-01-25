#!/bin/bash

article=$1;
article_in_harddisk=/mnt/sdb3/alama/itemization/$article;
article_in_ramdisk=/dev/shm/alama/itemization/$article;

if [[ ! -e $article_in_harddisk/text ]]; then
    echo "Failure: there is no text subdirectory for $article!";
    exit
fi

cp -R $article_in_harddisk $article_in_ramdisk;

cd $article_in_ramdisk;

num_items=`ls text/ckb*.miz | wc --lines`
for item_num in `seq 1 $num_items`; do
    item="text/ckb$item_num";
    accom -q -s -l $item > /dev/null 2>&1;
    if [[ $? -eq "0" ]]; then
	verifier -q -s -l $item > /dev/null 2>&1;
	if [[ $? -eq "0" ]]; then
	    exporter -q -s -l $item > /dev/null 2>&1;
	    transfer -q -s -l $item > /dev/null 2>&1;
	else
	    rm -Rf $article_in_ramdisk;
	    echo "Failure: $article item #$item_num: verifier errors";
	    exit 1;
	fi
    else
	rm -Rf $article_in_ramdisk;
	echo "Failure: $article item #$item_num: accommodator errors";
	exit 1;
    fi
done

rm -Rf $article_in_harddisk;
mv $article_in_ramdisk $article_in_harddisk;
echo "$article: ok"

exit 0;
