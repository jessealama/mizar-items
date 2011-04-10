#!/bin/bash -

mmllar=`cat $MIZFILES/mml.lar`;
itemization_source=/local/data/alama;

for article in $mmllar; do
    text_dir=$itemization_source/$article/text;
    if [[ -e $text_dir ]]; then
	num_miz=`find $itemization_source/$article/text -name 'ckb*.miz' | wc --lines`;
	num_xml=`find $itemization_source/$article/text -name 'ckb*.xml' | wc --lines`;
	if [[ ! "$num_miz" -eq "$num_xml" ]]; then
	    echo $article;
	fi
    else
	echo "Warning: article $article lacks a text subdirectory" 1>&2;
    fi
done

exit 0;
