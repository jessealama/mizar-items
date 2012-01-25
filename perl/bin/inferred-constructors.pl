#!/usr/bin/perl

use warnings;
use strict;
use File::Basename qw(dirname basename);
use Getopt::Long;
use Pod::Usage;
use XML::LibXML;
use File::Spec qw();
use Carp qw(croak);

my $xml_parser = XML::LibXML->new (suppress_warnings => 1,
				   suppress_errors => 1);

sub ensure_valid_xml_file {
  my $xml_path = shift;
  if (defined eval { $xml_parser->parse_file ($xml_path) }) {
    return 1;
  } else {
    croak ('Error: ', $xml_path, ' is not a well-formed XML file.');
  }
}

sub ensure_readable_file {
  my $file = shift;

  if (! -e $file) {
    croak ('Error: ', $file, ' does not exist.');
  }
  if (! -f $file) {
    croak ('Error: ', $file, ' is not a file.');
  }

  if (! -r $file) {
    croak ('Error: ', $file, ' is unreadable.');
  }

  return 1;
}

my $help = 0;
my $man = 0;
my $verbose = 0;
my $stylesheet_home = '/Users/alama/sources/mizar/xsl4mizar/items';

GetOptions('help|?' => \$help,
           'man' => \$man,
           'verbose'  => \$verbose,
	   'stylesheet-home=s' => \$stylesheet_home)
  or pod2usage(2);
pod2usage(1) if $help;
pod2usage(-exitstatus => 0, -verbose => 2) if $man;
pod2usage(1) unless (scalar @ARGV == 1);

my $article = $ARGV[0];

my $article_dirname = dirname ($article);
my $article_dirname_absolute = File::Spec->rel2abs ($article_dirname);
my $article_basename = basename ($article, '.miz');
my $article_basename_uc = uc $article_basename;
my $article_xml = "${article_dirname}/${article_basename}.xml";
my $article_absolute_xml = "${article_dirname}/${article_basename}.xml1";

my $absrefs_stylesheet = "${stylesheet_home}/addabsrefs.xsl";

ensure_readable_file ($absrefs_stylesheet);
ensure_readable_file ($article_xml);

# We need to first ensure that we have the absolute form of everything relevant
foreach my $extension ('xml', 'eno', 'dfs', 'ecl', 'eid', 'epr', 'erd', 'eth', 'esh') {
  my $xml_file = "${article_dirname}/${article_basename}.${extension}";
  my $absolutized_xml_file = "${article_dirname}/${article_basename}.${extension}1";
  if (-e $xml_file && ! -e $absolutized_xml_file) {
    my $xsltproc_status = system ("xsltproc $absrefs_stylesheet $xml_file > $absolutized_xml_file 2> /dev/null");
    my $xsltproc_exit_code = $xsltproc_status >> 8;
    if ($xsltproc_exit_code != 0) {
      croak ('Error: xsltproc did not exit cleanly applying the absolutizer stylesheet to ', $xml_file, '.', "\n");
    }
  }
}

# ensure_valid_xml_file ($article_xml);
ensure_valid_xml_file ($article_absolute_xml);

my $inferred_constructors_stylesheet = '/Users/alama/sources/mizar/xsl4mizar/items/inferred-constructors.xsl';

ensure_readable_file ($inferred_constructors_stylesheet);

my %inferred_constructors_table = ();

my @inferred_constructors = `xsltproc --stringparam 'article-directory' '${article_dirname_absolute}/' $inferred_constructors_stylesheet $article_absolute_xml`;
chomp @inferred_constructors;

foreach my $constructor (@inferred_constructors) {
  $inferred_constructors_table{$constructor} = 0;
}

# We may need to throw in equality, even if it is not mentioned
# anywhere.  Specifically, we may need its properties (reflexivity and
# symmetry).  Later on we can try to eliminate it.

$inferred_constructors_table{'hidden:rconstructor:1'} = 0;

print join ("\n", keys %inferred_constructors_table), "\n";
