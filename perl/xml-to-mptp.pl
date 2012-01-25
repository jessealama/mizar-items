#!/usr/bin/perl -w

use strict;

my $xml_fragment = shift (@ARGV);

$xml_fragment =~ m/([RFC])Cluster aid=\"([^\"]+)\" nr=\"([0-9]+)\"/;
my $kind = $1;
my $aid = lc $2;
my $nr = $3;

if ($kind eq "R") {
  print "r";
} elsif ($kind eq "F") {
  print "f";
} else {
  print "c";
}

print "c", $nr, "_", $aid, "\n";

exit 0;
