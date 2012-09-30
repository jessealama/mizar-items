#!/usr/bin/perl -w

use strict;

# Given a newline-to-nulled mizar article on standard input, rewrite
# "by" and "from" statements so that all null characters between the
# "by"/"from" are replaced by spaces.

my $line = <STDIN>;
chomp $line;
my $len = length $line;
my @chars = split (//, $line);

my $space = 0;
my $space_b = 0;
my $space_by = 0;
my $space_by_space = 0;

sub reset_by_scanner {
  $space = 0;
  $space_b = 0;
  $space_by = 0;
  $space_by_space = 0;
  return;
}

# already have my $space
my $space_f = 0;
my $space_fr = 0;
my $space_fro = 0;
my $space_from = 0;
my $space_from_space = 0;

sub reset_from_scanner {
  $space = 0;
  $space_f = 0;
  $space_fr = 0;
  $space_fr = 0;
  $space_fro = 0;
  $space_from = 0;
  $space_from_space = 0;
  return;
}

sub fix_bys {

  foreach my $i (0 .. $len - 1) {
    my $c = $chars[$i];

    if ($c eq ' ') {
      if ($space_by) {
	$space_by_space = 1;
      } else {
	$space = 1;	
      }
    }

    if ($c eq 'b') {
      if ($space) {
	$space_b = 1;
      } else {
	reset_by_scanner ();
      }
    }

    if ($c eq 'y') {
      if ($space_b) {
	$space_by = 1;
      } else {
	reset_by_scanner ();
      }
    }

    if ($c eq ';') {
      reset_by_scanner ();
    }

    if ($space_by_space) {
      if ($c eq "\0") {
	$chars[$i] = ' ';
      }
    }

  }
}

sub fix_froms {

  foreach my $i (0 .. $len - 1) {
    my $c = $chars[$i];

    if ($c eq ' ') {
      if ($space_from) {
	$space_from_space = 1;
      } else {
	$space = 1;	
      }
    }

    if ($c eq 'f') {
      if ($space) {
	$space_f = 1;
      } else {
	reset_from_scanner ();
      }
    }

    if ($c eq 'r') {
      if ($space_f) {
	$space_fr = 1;
      } else {
	reset_from_scanner ();
      }
    }

    if ($c eq 'o') {
      if ($space_fr) {
	$space_fro = 1;
      } else {
	reset_from_scanner ();
      }
    }

    if ($c eq 'm') {
      if ($space_fro) {
	$space_from = 1;
      } else {
	reset_from_scanner ();
      }
    }

    if ($c eq ';') {
      reset_from_scanner ();
    }

    if ($space_from_space) {
      if ($c eq "\0") {
	$chars[$i] = ' ';
      }
    }

  }
}

fix_bys ();
fix_froms ();

print join ('', @chars), "\n";

exit 0;
