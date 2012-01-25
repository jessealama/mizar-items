#!/usr/bin/perl

use strict;
use XML::LibXML;

my @item_kinds = ('Pattern', 'Definiens', '[RCF]Cluster', 'Constructor');
# - Definiens: gather these from from XML, compare to what appers in dfs
#
# - Identification: similar to Definiens.
#
# - Patterns: collect PIDs from the XML file, use that as the set of patterns;
# otherwise, use brute force
#
# - Constructor: take esh/eth; from these, gather the constructors.
# Might fail; if it does, use the full brute-force approach.  We might
# need to do ths recursively, invoving the result types of
# constructors, result type of result type, etc.

my %item_to_extension =
  (
   'Definiens' => 'dfs',
   '[RCF]Cluster' => 'ecl',
#   'CCluster' => 'ecl',
#   'FCluster' => 'ecl',
#   'Scheme' => 'esh',
   'Constructor' => 'atr',
#   'Theorem' => 'eth',
   'Identify' => 'eid',
   'Pattern' => 'eno',
  );

my $article = 'tarski';

my $ramdisk = "/dev/shm/alama/itemization";
my $harddisk = "/mnt/sdb3/alama/itemization";

# Make sure we don't max out the ramdisk
my @stuff_in_ramdisk = `find $ramdisk -mindepth 1 -maxdepth 1 -type d`;
if (@stuff_in_ramdisk > 20) {
  die "Failure: ramdisk is full; refusing to proceed";
}

# sanity check: article exists on the harddisk
my $article_on_harddisk = "$harddisk/$article";
unless (-e $article_on_harddisk) {
    die "$article doesn't exist under $harddisk!";
}

# check that this article is brutalizable: it has been fully itemized
unless (-e "$article_on_harddisk/prel") {
    die "Failure: article $article was not properly itemized!";
}

print "Brutalizing $article...", "\n";

# sanity check: the article isn't already in the ramdisk
my $article_in_ramdisk = "$ramdisk/$article";
if (-e $article_in_ramdisk) {
    die "Failure: $article is already in the ramdisk -- why?";
}

if (system ('cp', '-Rf', $article_on_harddisk, $ramdisk) != 0) {
  system ('rm', "-Rf", $article_in_ramdisk); # trash whatever is there
  die "Failure: error copying $article to the ramdisk: $!";
}

chdir $article_in_ramdisk or die "Unable to chdir to $article_in_ramdisk!";

my @items_for_article = `ls text/tarski*.miz | sed -e 's/\.miz//'`;
chomp @items_for_article;

foreach my $item (@items_for_article) {

  print "Brutalizing item $item of article $article...", "\n";

  print "Verifiying item...", "\n";
  
  my $verifier_time = 1;

  my $timeout = (int ($verifier_time * 2) + 1) . "s";

  print "Using a timeout of $timeout seconds", "\n";

  foreach my $item_kind (@item_kinds) {

    my $extension = $item_to_extension{$item_kind};
      
    if (-e "$item.$extension") {
      print "brutalizing item kind $item_kind for item $item of article $article...", "\n";
      my $exit_code = system ('/mnt/sdb3/alama/mizar-items/miz_item_deps_bf_tarski.pl', $item_kind, $extension, $item, $timeout);
	
      my $exit_code = $exit_code >> 8;
      my $err_message = $!;
      if ($exit_code == 0) {
	print "successfully minimized item kind $item_kind", "\n";
	# system ('cp', "$item.$extension", "$item-needed-$item_kind");
      } else {
	print "failure", "\n";
	system ('rm', "-Rf", "/dev/shm/alama/itemization/$article") == 0
	  or die "Failure: error deleting $article from the ramdisk!";
	die "Failure: something went wrong brutalizing item kind $item_kind for $item of $article: the exit code was $exit_code and the error output was: $err_message";
      }
    } else {
      print "Success: nothing to brutalize for items of kind $item_kind for item $item of article $article", "\n";
    }
  }
}

system ('rm', '-Rf', $article_on_harddisk);
system ('mv', $article_in_ramdisk, $harddisk) == 0
  or die "Failure: error moving $article out of the ramdisk!";

system ('touch', "$harddisk/$article-brutalization-stamp");

print "Success: brutalization of article $article succeeded", "\n";

exit 0;
