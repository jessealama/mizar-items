#!/bin/bash

article=$1;
host=`hostname -s`;
if [[ $host = "mizar" ]]; then
    harddisk=/local/data/proofs/itemization;
    ramdisk=/dev/shm;
    image=/home/alama/mizar-items/mizar;
    export PATH=/home/alama/mizsrc/7_11_07/bin:$PATH
    export MIZFILES=/home/alama/mizsrc/7_11_07;
elif [[ $host = "mws" ]]; then
    harddisk=/mnt/sdb3/alama/itemization;
    ramdisk=/dev/shm/alama/itemization;
    image=/mnt/sdb3/alama/mizar;
    export PATH=/mnt/sdb3/alama/7.11.07_4.156.1112/bin:$PATH
    export MIZFILES=/mnt/sdb3/alama/7.11.07_4.156.1112
else
    harddisk=/tmp
    ramdisk=/Volumes/ramdisk
    image=/Users/alama/sources/mizar/mizar-items/mizar;
fi

article_on_harddisk=$harddisk/$article;
article_in_ramdisk=$ramdisk/$article-1;

sbcl --disable-ldb \
     --noinform \
     --core $image \
     --eval '(in-package :mizar)' \
     --eval "(if (handler-bind ((warning #'muffle-warning)) (itemize-no-errors \"$article\")) (sb-ext:quit :unix-status 0) (sb-ext:quit :unix-status 1))"
     > /dev/null 2>&1;

if [[ $? -eq "0" ]]; then
    tarfile=$article.tar
    ramdisk_tarfile=$ramdisk/$tarfile;
    harddisk_tarfile=$harddisk/$tarfile;
    cd $ramdisk;
    tar cf $tarfile $article-1;
    gzip $tarfile;
    rm -f $harddisk_tarfile.gz
    mv $tarfile.gz $HOME;
    rm -Rf $article_in_ramdisk;
    exit 0;
else
    rm -Rf $article_in_ramdisk;
    echo "Failure: error itemizing $article";
    exit 1;
fi
