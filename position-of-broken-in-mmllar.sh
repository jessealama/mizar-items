#!/bin/bash -

no_text_subdir=`./check-text-subdir-exists.sh 2>/dev/null`;
unequal_miz_xml=`./check-num-xmls.sh 2>/dev/null`;

mmllar=$MIZFILES/mml.lar;

for article in $no_text_subdir $unequal_miz_xml; do
    grep --max-count=1 --line-number $article $mmllar;
done

exit 0;
