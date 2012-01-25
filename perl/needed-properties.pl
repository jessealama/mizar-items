#!/usr/bin/perl -w

use strict;

use File::Temp;
use Cwd;

my $cwd = getcwd ();

sub usage {
  print 'needed-properties.pl <article-name>', "\n";
  exit 0;
}

if (@ARGV != 1) {
  usage ();
}

my $article = $ARGV[0];

my $ramdisk = '/dev/shm';

sub check_directory {

  my $path = shift;

  if (! -e $path) {
    die "We expected to find a directory at '$path', but there is nothing there";
  }
  
  if (! -d $path) {
    die "The file at '$path', which we expect to be a directory, is not actually a directory";
  }

  if (! -r $path) {
    die "The directory at '$path' is not readable; we won't be able to do anything in it";
  }
  
  return;

}

sub check_executable_file {

  my $path = shift;

  if (! -e $path) {
    die "We expected to find an executable file at '$path', but there is nothing there";
  }
  
  if (! -f $path) {
    die "The file at '$path', which we expect to be an executable, is not actually a file";
  }

  if (! -r $path) {
    die "The file at '$path' is not readable";
  }

  if (! -x $path) {
    die "The file at '$path' is not executable";
  }
  
  return;

}

check_directory ($ramdisk);

my $brutalized_article_store = '/local/data/alama/brutalized-itemizations';

check_directory ($brutalized_article_store);

my $brutalized_article_dir = "$brutalized_article_store/$article";

check_directory ($brutalized_article_dir);

my @fragments = `find $brutalized_article_dir -type f -name "ckb*.miz"`;
chomp @fragments;
my $num_fragments = scalar @fragments;

my $needed_properties_for_fragment_script = '/home/alama/mizar-items/needed-properties-for-fragment.pl';

check_executable_file ($needed_properties_for_fragment_script);

my $article_tempdir_in_ramdisk = File::Temp->newdir (DIR => $ramdisk);

system ('cp', '-R', $brutalized_article_dir, $article_tempdir_in_ramdisk);

chdir "$article_tempdir_in_ramdisk/$article";

system ("find text -name 'ckb*.miz' | parallel --jobs +0 --keep-order $needed_properties_for_fragment_script {}");

chdir $cwd;

exit 0;
