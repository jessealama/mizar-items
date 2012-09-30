#!/usr/bin/perl -w

use strict;
use File::Basename;

sub usage {
  print 'needed-properties-for-fragment.pl <article-fragment-name>', "\n";
  exit 0;
}

if (@ARGV != 1) {
  usage ();
}

my $article_fragment = $ARGV[0];
my $article_basename = fileparse ($article_fragment, '.miz');
my $article_atr = "text/$article_basename.atr";
my $article_original_atr = "text/$article_basename.atr.orig";
my $article_temp_atr = "text/$article_basename.atr.temp";
my $article_pruned_atr = "$article_atr.pruned";

system ('cp', $article_atr, $article_original_atr);

my $strip_prop_stylesheet = '/home/alama/xsl4mizar/strip-prop.xsl';

sub property_needed_for_constructor {
  my $property = shift;
  my $constructor = shift;
  (my $kind, my $nr, my $aid, my $relnr) = split /-/, $constructor;
  system ("xsltproc --stringparam target_kind '$kind' --stringparam target_nr '$nr' --stringparam target_aid '$aid' --stringparam target_relnr '$relnr' --stringparam target_property '$property' $strip_prop_stylesheet $article_atr > $article_temp_atr 2> /dev/null") == 0
    or die "Something went wrong with applying the strip-property stylesheet with xsltproc";
  system ('mv', $article_temp_atr, $article_atr) == 0
    or die "Something went wrong moving '$article_temp_atr' to '$article_atr'";
  my $verifier_exit_code = system ("verifier -q -l -s $article_fragment > /dev/null 2> /dev/null");
  my $needed;
  if ($verifier_exit_code == 0) {
    $needed = 0;
  } else {
    $needed = 1;
  }
  system ('cp', $article_original_atr, $article_atr);
  return $needed;
}

my $propertied_constructors_stylesheet = '/home/alama/xsl4mizar/propertied-constructors.xsl';

my @constructors_with_properties = `xsltproc $propertied_constructors_stylesheet $article_pruned_atr | grep ' '`;
chomp @constructors_with_properties;

foreach my $constructor_with_properties (@constructors_with_properties) {
  (my $constructor, my @properties) = split / /, $constructor_with_properties;
  foreach my $property (@properties) {
    if (property_needed_for_constructor ($property, $constructor) == 1) {
      print $article_fragment, ' needs ', $property, ' of ', $constructor, "\n";
    } else {
      print $article_fragment, ' does not need ', $property, ' of ', $constructor, "\n";
    }
  }
}

exit 0;
