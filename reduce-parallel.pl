#!/usr/bin/perl -w

use strict;
use XML::LibXML;

my @articles = ();

while (defined (my $article = <STDIN>)) {
  chomp $article;
  push (@articles, $article);
}

# sanity: all the articles exist
foreach my $article (@articles) {
  my $article_on_harddisk = "/tmp/$article";
  unless (-e $article_on_harddisk) {
    die "The directory '$article_on_harddisk' for article '$article' doesn't exist!";
  }
  unless (-d $article_on_harddisk) {
    die "The directory '$article_on_harddisk' for article '$article' is not actually a directory!";
  }
  unless (-r $article_on_harddisk) {
    die "The directory '$article_on_harddisk' for article '$article' is not readable!";
  }
  my $article_text_dir = $article_on_harddisk . '/text';
  unless (-e $article_text_dir) {
    die "The text subdirectory '$article_text_dir' for article '$article' doesn't exist!";
  }
  unless (-d $article_text_dir) {
    die "The text subdirectory '$article_text_dir' for article '$article' is not actually a directory!";
  }
  unless (-r $article_text_dir) {
    die "The text subdirectory '$article_text_dir' for article '$article' is not readable!";
  }
  my @one_item = `find $article_text_dir -name 'ckb1.miz' | head -n 1`;
  if (@one_item == 0) {
    die "The text subdirectory '$article_text_dir' for article '$article' does not contain any items!";
  }
  # other sanity checks could be performed here, such as checking that all items are readable, but we'll skip that
}

# build the list of all items, which will eventually be passed to parallel
my @num_items = ();
foreach my $article (@articles) {
  my $article_on_harddisk = "/tmp/$article";
  my $num_items_for_article = `find $article_on_harddisk/text -name "ckb*.miz" | gwc --lines`;
  chomp $num_items_for_article;
  push (@num_items, $num_items_for_article);
}

# set up the ramdisk
my $ramdisk = "/Volumes/ramdisk";
my $harddisk = "/tmp";

# Make sure we don't max out the ramdisk
my @stuff_in_ramdisk = `find $ramdisk -mindepth 1 -maxdepth 1 -type d`;
if (@stuff_in_ramdisk > 20) {
  die "Failure: ramdisk is full; refusing to proceed";
}

# sanity check: the article isn't already in the ramdisk
foreach my $article (@articles) {
  my $article_in_ramdisk = "$ramdisk/$article";
  if (-e $article_in_ramdisk) {
    die "Failure: $article is already in the ramdisk -- why?";
  }
}

foreach my $article (@articles) {
  my $article_on_harddisk = "/tmp/$article";
  my $article_in_ramdisk = "$ramdisk/$article";
  if (system ('cp', '-Rf', $article_on_harddisk, $ramdisk) != 0) {
    system ('rm', "-Rf", $article_in_ramdisk); # trash whatever is there
    die "Failure: error copying $article to the ramdisk: $!";
  }
}

my $parallel_items = '';
foreach my $i (1 .. scalar @articles) {
  my $article = $articles[$i - 1];
  my $num_items_for_article = $num_items[$i - 1];
  foreach my $j (1 .. $num_items_for_article) {
    $parallel_items .= "$article:$j ";
  }
}

my $reduce_item_script = '/Users/alama/sources/mizar/mizar-items/reduce-item.pl';

# sanity
unless (-e $reduce_item_script) {
  die "Couldn't find the reduce-item script at '$reduce_item_script'!";
}
unless (-x $reduce_item_script) {
  die "The reduce-item script at '$reduce_item_script' is not executable!";
}

my $parallel_exit_code 
  = system ("parallel --jobs +0 $reduce_item_script {} > /dev/null 2>&1 ::: $parallel_items");
$parallel_exit_code = $parallel_exit_code >> 8;
unless ($parallel_exit_code == 0) {
  die "parallel did not exit cleanly: its exit code was $parallel_exit_code"
}

print "Success: brutalization of articles @articles succeeded", "\n";

foreach my $article (@articles) {
  my $article_on_harddisk = "/tmp/$article";
  my $article_in_ramdisk = "$ramdisk/$article";
  system ('rm', '-Rf', $article_on_harddisk);
  system ('mv', $article_in_ramdisk, $harddisk) == 0
    or die "Failure: error moving $article out of the ramdisk!";
}

exit 0;
