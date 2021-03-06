#!/usr/bin/perl -w

use strict;
use File::Copy qw(copy move);

######################################################################
## External data upon which we depend
######################################################################

my $brutalized_articles_dir = '/local/data/alama/brutalized-itemizations';

if (-e $brutalized_articles_dir) {
  die 'Error: the directory containing the brutalized articles could not be found at the expected location (', $brutalized_articles_dir, ').';
}

if (! -d $brutalized_articles_dir) {
  die 'Error: we expected to find a directory at ', $brutalized_articles_dir, '.';
}

my $promote_constructor_stylesheet = 'promote.xsl';

if (! -e $promote_constructor_stylesheet) {
  die 'Error: the promote-constructor stylesheet does not exist at the expected location (', $promote_constructor_stylesheet, ').';
}

if (! -r $promote_constructor_stylesheet) {
  die 'Error: the promote-constructor stylesheet is unreadable.';
}

my $dependency_table = 'mizar-needed.mizar-items.no-patterns';

if (! -e $dependency_table) {
  die 'Error: the mizar-items dependency table does not exist at the expected location (', $dependency_table, ').';
}

if (! -r $dependency_table) {
  die 'Error: the mizar-items dependency is unreadable.';
}

my $item_to_fragment_table = 'item-to-fragment-table';

if (! -e $item_to_fragment_table) {
  die 'The item-to-fragment table could not be found at the expected location (', $item_to_fragment_table, ').';
}

if (! -r $item_to_fragment_table) {
  die 'The item-to-fragment table at ', $item_to_fragment_table, ' is unreadable.';
}

if (scalar @ARGV != 1) {
  print 'Usage: needed-constructors.pl ITEM-IN-MIZAR-ITEMS-NOTATION';
  exit 1;
}

my $item = $ARGV[0];

my $item_article = undef;
my $item_kind = undef;
my $item_number = undef;

if ($item =~ /([a-z0-9_]+):([^:]+):([0-9]+)/) {
  ($item_article, $item_kind, $item_number) = ($1, $2, $3);
} else {
  die "'${item}' doesn't appear to be in mizar-items notation.";
}

my @constructor_deps = `grep "^${item} \|^${item}\$" $dependency_table | tr ' ' '\n' | tail -n +2 | grep ':.constructor:'`;
chomp @constructor_deps;

# DEBUG
warn $item, ' has ', scalar @constructor_deps, ' constructor dependencies.';

# Resolve the CKB from which $item comes
my @fragment_numbers = `grep "^${item} " $item_to_fragment_table | cut -f 2 -d ' '`;
chomp @fragment_numbers;

if (scalar @fragment_numbers == 0) {
  die 'Error: we are unable to determine the number of the fragment of ', $item_article, ' from which ', $item, ' comes.';
} elsif (scalar @fragment_numbers > 1) {
  die 'Error: ', $item, ' apparently comes from multiple sources.';
}

my $fragment_number = $fragment_numbers[0];

my $brutalized_article_dir = "${brutalized_articles_dir}/${item_article}";

if (! -e $brutalized_article_dir) {
  die 'Error: the directory for ', $item_article, ' under ', $brutalized_articles_dir, ' does not exist.';
}

if (! -d $brutalized_article_dir) {
  die 'Error: the directory for ', $item_article, ' under ', $brutalized_articles_dir, ' does not appear to be a directory.';
}

my $brutalized_article_text_subdir = "${brutalized_article_dir}/text";

if (! -e $brutalized_article_text_subdir) {
  die 'Error: the text subdirectory for the itemized article ', $item_article, ' under ', $brutalized_articles_dir, ' does not exist.';
}

if (! -d $brutalized_article_text_subdir) {
  die 'Error: the text subdirectory for ', $item_article, ' under ', $brutalized_articles_dir, ' does not appear to be a directory.';
}

my $item_miz = "${brutalized_article_text_subdir}/ckb${fragment_number}.miz";

if (! -e $item_miz) {
  die 'Error: the .miz file corresponding to ', $item, ' does not exist at the expected location (', $item_miz, ').';
}

if (! -f $item_miz) {
  die 'Error: the .miz file corresponding to ', $item, ' does not exist at the expected location (', $item_miz, ').';
}

if (! -r $item_miz) {
  die 'Error: the .miz file corresponding to ', $item, ' (,', $item_miz, ') is unreadable.';
}

my $item_atr = "${brutalized_article_text_subdir}/ckb${fragment_number}.atr";

if (! -e $item_atr) {
  die 'Error: the .atr file corresponding to ', $item, ' does not exist at the expected location (', $item_atr, ').';
}

if (! -f $item_atr) {
  die 'Error: the .atr file corresponding to ', $item, ' does not exist at the expected location (', $item_atr, ').';
}

if (! -r $item_atr) {
  die 'Error: the .atr file corresponding to ', $item, ' (,', $item_atr, ') is unreadable.';
}

my $item_atr_orig = "${brutalized_article_text_subdir}/ckb${fragment_number}.atr.orig";

copy ($item_atr, $item_atr_orig)
  or die 'Error copying ', $item_atr, ' to ', $item_atr_orig, ': ', $!;

my $item_atr_tmp = "${brutalized_article_text_subdir}/ckb${fragment_number}.atr.tmp";
my $item_err = "${brutalized_article_text_subdir}/ckb${fragment_number}.err";

# Now we finally do something
my %promotable_constructors = ();

foreach my $constructor (@constructor_deps) {
  my $xsltproc_promote_status = system ("xsltproc --stringparam constructors ',${constructor},' --output $item_atr_tmp $promote_constructor_stylesheet $item_atr");
  my $xsltproc_promote_exit_status = $xsltproc_promote_status >> 8;
  if ($xsltproc_promote_exit_status != 0) {
    copy ($item_atr, $item_atr_orig)
      or die 'Error: xsltproc did not exit cleanly when promoting ', $constructor, ', and we just failed to restore the original .atr file: ', $!;
    die 'Error: xsltproc did not exit cleanly when promoting ', $constructor, '.';
  }
  my $verifier_status = system ("verifier -q -s -l ${item_miz} > /dev/null 2>&1");
  my $verifier_exit_code = $verifier_status >> 8;
  if ($verifier_exit_code == 0 && -z $item_err) {
    $promotable_constructors{$constructor} = 0;
  }
  copy ($item_atr_orig, $item_atr)
    or die 'Error: we failed to restore the original .atr file for ', $item, ': ', $!;
}

foreach my $constructor (keys %promotable_constructors) {
  print $constructor, "\n";
}
