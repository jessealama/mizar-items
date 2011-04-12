#!/bin/bash -

for dir in `ls`; do
 echo $dir;
 cd $dir;
 find text -name "ckb*.xml1" ! -empty | parallel --jobs +0 "xsltproc --param linking 's' --param colored '1' --param proof_links '1' --param ajax_proofs '1' --stringparam source_article '$dir' ~/sources/mizar/xsl4mizar/MHTML/mhtml_main.xsl {} > {.}.html";
  cd ..;
done