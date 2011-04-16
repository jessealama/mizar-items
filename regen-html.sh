#!/bin/bash -

html_stylesheet=~/sources/mizar/xsl4mizar/MHTML/mhtml_main.xsl
cwd=`pwd`;

# sanity
if [ ! -e $html_stylesheet ]; then
    echo "Error: the mizar HTML stylesheet cannot be found at the expected location '$html_stylesheet'" 1>&2;
    exit 1;
fi
if [ ! -r $html_stylesheet ]; then
    echo "Error: the mizar HTML stylesheet at '$html_stylesheet' is unreadable" 1>&2;
    exit 1;
fi

for article in hidden tarski `cat $MIZFILES/mml.lar`; do
 article_dir="$cwd/$article";
 echo $article;
 cd $article_dir;
 if [ $? = '0' ]; then
     article_text_dir="$article/text";
     if [ -e $article_text_dir ]; then
	 find text -name "ckb*.xml1" ! -empty | parallel --jobs +0 "xsltproc --param linking 's' --param colored '1' --param mizar_items '1' --param proof_links '1' --param ajax_proofs '0' --stringparam source_article '$article' $html_stylesheet {} > {.}.html";
     else
	 echo "Error: article $article lacks a text subdirectory" 1>&2;
  else
     echo "Error: cannot change diectory to '$article_dir' for $article" 1>&2;
 fi
done

exit 0;
