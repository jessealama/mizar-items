#!/bin/bash

article=$1;

cp -R /mnt/sdb3/alama/itemization/$article /dev/shm/alama/itemization/$article;
cd /dev/shm/alama/itemization/$article;
items=`ls text/*.miz`;
for item in $items; do
    accom -q -s -l $item > /dev/null 2> /dev/null ;
    if [[ $? -eq "0" ]]; then
	verifier -q -s -l $item > /dev/null 2>/dev/null;
	if [[ $? -eq "0" ]]; then
	    echo "$article:$item: ok";
	else
	    echo "$article:$item: not ok (verifier error)";
	fi;
    else
	echo "$article:$item: not ok (accommodator error)";
    fi;
done
rm -Rf /mnt/sdb3/alama/itemization/$article;
mv /dev/shm/alama/itemization/$article /mnt/sdb3/alama/itemization/$article;

exit 0;
