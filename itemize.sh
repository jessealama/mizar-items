#!/bin/bash

article=$1;
harddisk=/mnt/sdb3/alama/itemization;
ramdisk=/dev/shm/alama/itemization;
article_on_harddisk=$harddisk/$article;
article_in_ramdisk=$ramdisk/$article-1;

sbcl --disable-ldb \
     --dynamic-space-size 16000 \
     --noinform \
     --core mizar \
     --eval '(in-package :mizar)' \
     --eval "(handler-bind ((warning #'muffle-warning)) (itemize-no-errors \"$article\"))" \
     --eval '(sb-ext:quit)' \
     > /dev/null 2>&1;

if [[ $? -eq "0" ]]; then
    rm -Rf $article_on_harddisk;
    mv $article_in_ramdisk $article_on_harddisk;
    touch $harddisk/$article-itemization-stamp;
else
    rm -Rf $article_in_ramdisk;
    echo "Failure: lisp died while itemizing $article";
fi

exit 0;