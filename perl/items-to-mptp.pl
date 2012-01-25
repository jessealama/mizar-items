#!/usr/bin/perl -w

use strict;

my $item = shift (@ARGV);

# sanity check
unless (-e $item) {
  die 'We cannot compute the MPTP name of a non-existent item';
}
if (-z $item) {
  die 'We cannot compute the MPTP name of an empty item';
}

# we are sane (I think)

my $second_line = `head -n 2 $item | tail -n 1`;
chomp $second_line;

if ($second_line =~ /SchemeBlock/) {
  print "s1";
}

if ($second_line =~ /IdentifyRegistration/) {
  print "i1";
}

if ($second_line =~ /RegistrationBlock/) {
  my $fourth_line = `head -n 4 $item | tail -n 1`;
  chomp $fourth_line;
  if ($fourth_line =~ /CCluster/) {
    print "cc1";
  } elsif ($fourth_line =~ /FCluster/) {
    print "fc1";
  } else {
    print "rc1";
  }

}

if ($second_line =~ /DefinitionBlock/) {
  print "definition";
}

if ($second_line =~ /NotationBlock/) {
  print "notation";
}

if ($second_line =~ /JustifiedTheorem/) {
  $second_line =~ m/nr=\"([0-9]+)\"/;
  my $theorem_nr = $1;
  print "t$theorem_nr";
}

if ($second_line =~ /Proposition/) {
  print "proposition";
}

print "\n";

exit 0;
