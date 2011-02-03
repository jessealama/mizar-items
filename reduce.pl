#!/usr/bin/perl

use strict;

my %item_to_extension =
  (
   'Definiens' => 'dfs',
   'RCluster' => 'ecl',
   'CCluster' => 'ecl',
   'FCluster' => 'ecl',
   'Scheme' => 'esh',
   'Constructor' => 'atr',
   'Theorem' => 'eth',
   'Identify' => 'eid',
   'Pattern' => 'eno',
  );

my $article = $ARGV[0];

my $ramdisk = "/dev/shm/alama/itemization";

my @stuff_in_ramdisk = `find $ramdisk -mindepth 1 -maxdepth 1 -type d`;

if (@stuff_in_ramdisk > 10) {
  die "Failure: ramdisk is full; refusing to proceed";
}

# check that this article is brutalizable: it has been fully itemized
unless (-e "/mnt/sdb3/alama/itemization/$article/prel") {
    die "Failure: article $article was not properly itemized!";
}

print "Brutalizing $article...", "\n";

system ('cp', '-Rf', "/mnt/sdb3/alama/itemization/$article", "/dev/shm/alama/itemization") == 0
  or die "Failure: error copying $article to the ramdisk!";

chdir "/dev/shm/alama/itemization/$article";

my @items_for_article = `ls text/*.miz | sed -e 's/\.miz//'`;
chomp @items_for_article;

foreach my $item (@items_for_article) {

  print "Brutalizing item $item of article $article...", "\n";

  foreach my $item_kind (keys %item_to_extension) {

    my $extension = $item_to_extension{$item_kind};

    if (-e "$article.$extension") {
      my $exit_code = system ('timeout', '2m', '/mnt/sdb3/alama/mizar-items/miz_item_deps_bf.pl', $item_kind, $extension, $item);
      my $exit_code = $exit_code >> 8;
      my $err_message = $!;
      if ($exit_code == 0) {
	print "successfully minimized item kind $item_kind", "\n";
	system ('cp', "$article.$extension", "$article-needed-$item_kind");
      } elsif ($exit_code == 124) {
	print "failure", "\n";
	system ('rm', "-Rf", "/dev/shm/alama/itemization/$article") == 0
	  or die "Failure: Error deleting $article from the ramdisk!";
	die "Failure: timeout brutalizing item kind $item_kind for item $item of $article!";
      } else {
	print "failure", "\n";
	system ('rm', "-Rf", "/dev/shm/alama/itemization/$article") == 0
	  or die "Failure: error deleting $article from the ramdisk!";
	die "Failure: something went wrong brutalizing item kind $item_kind for $item of $article: the exit code was $exit_code and the error output was: $err_message";
      }
    } else {
      print "nothing to trim for items of kind $item_kind", "\n";
    }
  }
}

system ('rm', '-Rf', "/mnt/sdb3/alama/itemization/$article") == 0
  or die "Failure: error removing $article from /mnt/sdb3/alama/itemization!";
system ('mv', "/dev/shm/alama/itemization/$article", "/mnt/sdb3/alama/itemization/$article") == 0
  or die "Failure: error moving $article out of the ramdisk!";

print "Success: brutalization of article $article succeeded", "\n";

exit 0;
