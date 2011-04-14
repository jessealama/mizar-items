#!/bin/bash -

html_stylesheet=~/sources/mizar/xsl4mizar/MHTML/mhtml_main.xsl

for article in hidden tarski `cat $MIZFILES/mml.lar`; do
 echo $article;
 cd $article;
 find text -name "ckb*.xml1" ! -empty | parallel --jobs +0 "xsltproc --param linking 's' --param colored '1' --param mizar_items '1' --param proof_links '1' --param ajax_proofs '0' --stringparam source_article '$article' $html_stylesheet {} > {.}.html";
  cd ..;
done

exit 0;
