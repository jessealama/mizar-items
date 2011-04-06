#!/bin/sh -

mmllar=`cat $MIZFILES/mml.lar`;
itemization_source=/local/data/alama;

for article in $mmllar; do
    num_miz=`find $itemization_source/$article/text -name 'ckb*.miz' | wc --lines`;
    num_xml=`find $itemization_source/$article/text -name 'ckb*.xml' | wc --lines`;
    if [[ ! "$num_miz" -eq "$num_xml" ]]; then
	echo $article;
    fi
done

exit 0;
