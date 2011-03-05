#!/bin/bash

article=$1;
host=`hostname -s`;
if [[ $host -eq "mizar" ]]; then
    harddisk=/local/data/proofs/itemization;
    ramdisk=/dev/shm;
    image=/home/alama/mizar-items/mizar;
else
    harddisk=/mnt/sdb3/alama/itemization;
    ramdisk=/dev/shm/alama/itemization;
    image=/mnt/sdb3/alama/mizar;
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
else
    rm -Rf $article_in_ramdisk;
    echo "Failure: error itemizing $article";
fi

exit 0;
