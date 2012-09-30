#!/usr/bin/perl -w

use strict;

# Take XML lines such as
#
# <Scheme articlenr="1" nr="1" aid="TARSKI">
#
# where aid is TARSKI and output
#
# tarski:scheme:1
#
# Take lines such as
#
# <Theorem articlenr="4" nr="1" aid="CKB5" kind="D" constrkind="K" constrnr="6">
#
# and output "CKB5", alone.  Same for lines such as
#
# <Theorem articlenr="15" nr="1" aid="CKB23" kind="T">
#
# which would give us "CKB23".
#
# In other words, look at the value of the AID attribute.  If it is
# "TARSKI", then look at the kind of element it is and its number.  If
# the AID attribute is not "tarski", then simply output it.
#
# The lines are read from standard input.

while (defined (my $line = <STDIN>)) {
  chomp $line;
  $line =~ /aid=\"([^\"]+)\"/;
  my $aid = $1;
  if ($aid =~ /CKB[0-9]+/) {
    print $aid, "\n";
  } else {
    my $aid_lc = lc $aid;
    $line =~/<(\w+)/;
    my $element_name = $1;
    $line =~ /nr=\"([0-9]+)\"/;
    my $nr = $1;
    print $aid_lc, ':', $element_name, ':', $nr, "\n";
  }
}

exit 0;
