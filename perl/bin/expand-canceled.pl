#!/usr/bin/perl -w

use strict;

while (defined (my $line = <STDIN>)) {
  chomp $line;
  if ($line =~ m/( *)canceled ([0-9]+);(.*)/) {
    my ($before, $num_canceled, $after) = ($1,$2,$3);
    foreach my $i (1 .. $num_canceled) {
      print $before, 'canceled;', "\n";
    }
    print $before, $after, "\n";
  } else {
    print $line, "\n";
  }
}

exit 0;
