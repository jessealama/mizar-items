#!/usr/bin/perl -w

use strict;
use File::Copy qw(copy move);

######################################################################
## External data upon which we depend
######################################################################

my $brutalized_lemmas_dir = '/local/data/alama/mizar-items/lemmas';

if (! -e $brutalized_lemmas_dir) {
  die 'Error: the directory containing the brutalized articles could not be found at the expected location (', $brutalized_lemmas_dir, ').';
}

if (! -d $brutalized_lemmas_dir) {
  die 'Error: we expected to find a directory at ', $brutalized_lemmas_dir, '.';
}

my $promote_constructor_stylesheet = '/local/data/alama/mizar-items/promote.xsl';

if (! -e $promote_constructor_stylesheet) {
  die 'Error: the promote-constructor stylesheet does not exist at the expected location (', $promote_constructor_stylesheet, ').';
}

if (! -r $promote_constructor_stylesheet) {
  die 'Error: the promote-constructor stylesheet is unreadable.';
}

my $lemma = $ARGV[0];
chomp $lemma;

my $lemma_article = undef;
my $lemma_number = undef;

if ($lemma =~ /^l([0-9]+)_([a-z0-9_]+)$/) {
  ($lemma_number, $lemma_article) = ($1, $2);
} else {
  die "'${lemma}' doesn't appear to be an MPTP lemma.";
}

my $dependency_table = "${brutalized_lemmas_dir}/${lemma_article}.deps";

if (! -e $dependency_table) {
  die 'Error: the mizar-items dependency table does not exist at the expected location (', $dependency_table, ').';
}

if (! -r $dependency_table) {
  die 'Error: the mizar-items dependency is unreadable.';
}

if (scalar @ARGV != 1) {
  print 'Usage: promotable-constructors.pl ITEM-IN-MIZAR-ITEMS-NOTATION', "\n";
  exit 1;
}

my @constructor_deps = `grep "^${lemma} " $dependency_table | tr -s ' ' | tr ' ' '\n' | tail -n +2 | grep ':.constructor:'`;
chomp @constructor_deps;

my $brutalized_article_dir = "${brutalized_lemmas_dir}/${lemma_article}";

if (! -e $brutalized_article_dir) {
  die 'Error: the directory for ', $lemma_article, ' under ', $brutalized_lemmas_dir, ' does not exist.';
}

if (! -d $brutalized_article_dir) {
  die 'Error: the directory for ', $lemma_article, ' under ', $brutalized_lemmas_dir, ' does not appear to be a directory.';
}

my $lemma_miz = "${brutalized_article_dir}/l${lemma_number}.miz";

if (! -e $lemma_miz) {
  die 'Error: the .miz file corresponding to ', $lemma, ' does not exist at the expected location (', $lemma_miz, ').';
}

if (! -f $lemma_miz) {
  die 'Error: the .miz file corresponding to ', $lemma, ' does not exist at the expected location (', $lemma_miz, ').';
}

if (! -r $lemma_miz) {
  die 'Error: the .miz file corresponding to ', $lemma, ' (,', $lemma_miz, ') is unreadable.';
}

my $lemma_atr = "${brutalized_article_dir}/l${lemma_number}.atr";

if (! -e $lemma_atr) {
  die 'Error: the .atr file corresponding to ', $lemma, ' does not exist at the expected location (', $lemma_atr, ').';
}

if (! -f $lemma_atr) {
  die 'Error: the .atr file corresponding to ', $lemma, ' does not exist at the expected location (', $lemma_atr, ').';
}

if (! -r $lemma_atr) {
  die 'Error: the .atr file corresponding to ', $lemma, ' (,', $lemma_atr, ') is unreadable.';
}

my $lemma_atr_orig = "${brutalized_article_dir}/l${lemma_number}.atr.orig";

copy ($lemma_atr, $lemma_atr_orig)
  or die 'Error copying ', $lemma_atr, ' to ', $lemma_atr_orig, ': ', $!;

my $lemma_atr_tmp = "${brutalized_article_dir}/l${lemma_number}.atr.tmp";
my $lemma_err = "${brutalized_article_dir}/l${lemma_number}.err";

# Now we finally do something
chdir $brutalized_article_dir;
my %promotable_constructors = ();
my %unpromotable_constructors = ();

foreach my $constructor (@constructor_deps) {
  my $xsltproc_promote_status = system ("xsltproc --stringparam constructors ',${constructor},' --output $lemma_atr_tmp $promote_constructor_stylesheet $lemma_atr");
  my $xsltproc_promote_exit_status = $xsltproc_promote_status >> 8;
  if ($xsltproc_promote_exit_status != 0) {
    copy ($lemma_atr, $lemma_atr_orig)
      or die 'Error: xsltproc did not exit cleanly when promoting ', $constructor, ', and we just failed to restore the original .atr file: ', $!;
    die 'Error: xsltproc did not exit cleanly when promoting ', $constructor, '.';
  }
  copy ($lemma_atr_tmp, $lemma_atr)
    or die 'Error: we failed to copy the transformed .atr (in which ', $constructor, ' is promoted) to the real .atr.';
  my $verifier_status = system ("verifier -c -q -s -l ${lemma_miz} > /dev/null 2>&1");
  my $verifier_exit_code = $verifier_status >> 8;
  if ($verifier_exit_code == 0 && -z $lemma_err) {
    # warn $constructor, ' can be safely promoted for ', $lemma;
    $promotable_constructors{$constructor} = 0;
  } else {
    $unpromotable_constructors{$constructor} = 0;
    # DEBUG

    # warn $constructor, ' cannot be safely promoted.';

    # warn 'verifier did not like it when we tried to promote ', $constructor, '; here is the .err file:';
    # my @errs = `cat $lemma_err`;
    # chomp @errs;
    # foreach my $err (@errs) {
    #   warn $err;
    # }
  }
  copy ($lemma_atr_orig, $lemma_atr)
    or die 'Error: we failed to restore the original .atr file for ', $lemma, ': ', $!;
}

foreach my $constructor (keys %unpromotable_constructors) {
  print $constructor, "\n";
}
