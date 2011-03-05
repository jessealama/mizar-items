#!/bin/bash

article=$1;
host=`hostname -s`;
if [[ $host = "mizar" ]]; then
    harddisk=/local/data/proofs/itemization;
    ramdisk=/dev/shm;
    image=/home/alama/mizar-items/mizar;
    export PATH=/home/alama/mizsrc/7_11_07/bin:$PATH
    export MIZFILES=/home/alama/mizsrc/7_11_07;
else
    harddisk=/mnt/sdb3/alama/itemization;
    ramdisk=/dev/shm/alama/itemization;
    image=/mnt/sdb3/alama/mizar;
    export PATH=/mnt/sdb3/alama/7.11.07_4.156.1112/bin:$PATH
    export MIZFILES=/mnt/sdb3/alama/7.11.07_4.156.1112
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
    bzip2 $tarfile;
    rm -f $harddisk_tarfile.bz2
    mv $tarfile.bz2 $HOME;
    rm $article_in_ramdisk
else
    rm -Rf $article_in_ramdisk;
    echo "Failure: error itemizing $article";
fi

exit 0;
