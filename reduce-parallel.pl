#!/usr/bin/perl -w

use strict;
use XML::LibXML;

chdir $article_in_ramdisk or die "Unable to chdir to $article_in_ramdisk!";

my @items_for_article = `ls text/ckb*.miz | sed -e 's/\.miz//'`;
chomp @items_for_article;

`parallel --jobs +0 reduce-item.pl {} :::: @items_for_article`;

system ('touch', "$harddisk/$article-brutalization-stamp");

print "Success: brutalization of article $article succeeded", "\n";

exit 0;
