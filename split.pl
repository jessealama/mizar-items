#!/usr/bin/perl -w

use strict;
use File::Basename qw(basename dirname);
use File::Copy;

if (scalar @ARGV != 2) {
  print 'Usage: split.pl ARTICLE WORK-DIRECTORY', "\n";
  exit 1;
}

my $article_basename = basename ($ARGV[0], '.miz');
my $article_miz = "$article_basename.miz";
my $article_dirname = dirname ($ARGV[0]);

my $article = "$article_dirname/$article_miz";

my $work_directory = $ARGV[1];

# Sanity check: the given article file exists and is readable, the
# given directory doesn't exist

if (! -e $article) {
  print 'Error: the given article ', $article, ' does not exist.', "\n";
  exit 1;
}

if (! -r $article) {
  print 'Error: the given article ', $article, ' is unreadable.', "\n";
  exit 1;
}

if (-e $work_directory) {
  print "Error: '$work_directory' already exists.", "\n";
  exit 1;
}

my $split_stylesheet = '/Users/alama/sources/mizar/xsl4mizar/items/split-simple.xsl';
my $wsm_stylesheet = '/Users/alama/sources/mizar/xsl4mizar/items/wsm.xsl';

# Sanity check: the stylesheets exists and are readable

if (! -f $split_stylesheet) {
  print 'Error: the stylesheet used to split articles does not exist at the expected location', "\n", "\n", '  ', $split_stylesheet, "\n";
  exit 1;
}

if (! -r $split_stylesheet) {
  print 'Error: the stylesheet used to split articles at', "\n", "\n", '  ', $split_stylesheet, 'is unreadable.', "\n";
  exit 1;
}

if (! -f $wsm_stylesheet) {
  print 'Error: the stylesheet used to generate .miz files does not exist at the expected location', "\n", "\n", '  ', $wsm_stylesheet, "\n";
  exit 1;
}

if (! -r $wsm_stylesheet) {
  print 'Error: the stylesheet used to generate .miz file at', "\n", "\n", '  ', $wsm_stylesheet, 'is unreadable.', "\n";
  exit 1;
}

# Check that accom and newparser are available

my $which_accom_status = system ("which accom > /dev/null 2>&1");
my $which_accom_exit_code = $which_accom_status >> 8;

if ($which_accom_exit_code != 0) {
  print 'Error: accom appears to be unavailable.  Check your PATH.', "\n";
  exit 1;
}

my $which_newparser_status = system ("which newparser > /dev/null 2>&1");
my $which_newparser_exit_code = $which_newparser_status >> 8;

if ($which_newparser_exit_code != 0) {
  print 'Error: newparser appears to be unavailable.  Check your PATH.', "\n";
  exit 1;
}

mkdir $work_directory
  or (print ('Error: unable to create the work directory at ', $work_directory, "\n") && exit 1);

# Make the .wsx

my $article_in_work_directory = "$work_directory/$article_miz";
my $original_article_in_work_directory = "$work_directory/$article_miz.orig";
my $article_wsx = "$article_basename.wsx";
my $article_wsx_split = "$article_basename.wsx.splt";
my $wsx_in_work_directory = "$work_directory/$article_wsx";
my $wsx_split_in_work_directory = "$work_directory/$article_wsx";

copy ($article, $article_in_work_directory)
  or (print "Error: unable to copy\n\n  $article\n\nto\n\n  $article_in_work_directory\n" && exit 1);
copy ($article, $original_article_in_work_directory)
  or (print "Error: unable to copy\n\n  $article\n\nto\n\n  $original_article_in_work_directory\n" && exit 1);

my $accom_status = system ("accom -q -l $article_in_work_directory > /dev/null 2>&1");
my $accom_exit_code = $accom_status >> 8;

if ($accom_exit_code != 0) {
  print "Error: something went wrong calling accom on\n\n  $article_in_work_directory\n";
  exit 1;
}

my $newparser_status = system ("newparser -q -l $article_in_work_directory > /dev/null 2>&1");
my $newparser_exit_code = $newparser_status >> 8;

if ($newparser_exit_code != 0) {
  print "Error: something went wrong calling newparser on\n\n  $article_in_work_directory\n";
  exit 1;
}

# Split the original .miz (i.e., the .wsx)

my $xsltproc_status = system ("xsltproc --output $wsx_split_in_work_directory $split_stylesheet $wsx_in_work_directory 2> /dev/null");
my $xsltproc_exit_code = $xsltproc_status >> 8;

if ($xsltproc_exit_code != 0) {
  print "Error: something went wrong calling xsltproc with the stylesheet\n\n  $split_stylesheet\n\non the .wsx file at\n\n  $wsx_in_work_directory\n";
  exit 1;
}

# Sanity check: the split wsx we just created exists

if (! -e $wsx_split_in_work_directory) {
  print "Error: somehow the XML file that we intended to create,\n\n  $wsx_split_in_work_directory,\n\ndoesn't exist.\n";
  exit 1;
}

# Make the split .miz

$xsltproc_status = system ("xsltproc --output $article_in_work_directory $wsm_stylesheet $wsx_in_work_directory 2> /dev/null");
$xsltproc_exit_code = $xsltproc_status >> 8;

if ($xsltproc_exit_code != 0) {
  print "Error: something went wrong calling xsltproc with the stylesheet\n\n  $wsm_stylesheet\n\non the .wsx file at\n\n  $wsx_in_work_directory\n";
  exit 1;
}



exit 0;

