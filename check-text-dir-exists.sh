#!/bin/bash -

mmllar=`cat $MIZFILES/mml.lar`;
itemization_source=/local/data/alama;

for article in $mmllar; do
    text_dir=$itemization_source/$article/text;
    if [[ ! -e $text_dir ]]; then
	echo $article;
    fi
done

exit 0;
