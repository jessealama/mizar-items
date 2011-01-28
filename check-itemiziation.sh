#!/bin/bash

article=$1;

cp -R /mnt/sdb3/alama/itemization/$article /dev/shm/alama/itemization/$article;
cd /dev/shm/alama/itemization/$article
num_items=`ls text/*.miz | wc --lines`
for item_num in `seq 1 $num_items`; do
    item="text/ckb$item_num";
    accom -q -s -l $item > /dev/null 2>&1;
    if [[ $? -eq "0" ]]; then
	verifier -q -s -l $item > /dev/null 2>&1;
	if [[ $? -eq "0" ]]; then
	    exporter -q -s -l $item > /dev/null 2>&1;
	    transfer -q -s -l $item > /dev/null 2>&1;
	else
	    rm -Rf /dev/shm/alama/itemization/$article;
	    echo "$article item #$item_num: verifier errors";
	    exit 1;
	fi
    else
	rm -Rf /dev/shm/alama/itemization/$article;
	echo "$article item #$item_num: accommodator errors";
	exit 1;
    fi
done

rm -Rf /mnt/sdb3/alama/itemization/$article;
mv /dev/shm/alama/itemization/$article /mnt/sdb3/alama/itemization/$article;
echo "$article: ok"

exit 0;
