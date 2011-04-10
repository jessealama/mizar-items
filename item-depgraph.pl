#!/usr/bin/perl -w

use strict;

use Getopt::Long;

my $fragment_depgraph_file
  = '/Users/alama/sources/mizar/mizar-items/ckb-ckb-depgraph';
my $item_to_fragment_table_file
  = '/Users/alama/sources/mizar/mizar-items/mizar-item-ckb-table';

my $lisp_output;
my $reverse_fragment_depgraph;
my $compute_tc;

GetOptions (
	    "fragment-dependency-graph=s" => \$fragment_depgraph_file,
	    "item-to-fragment-table=s" => \$item_to_fragment_table_file,
	    "lisp-output" => \$lisp_output,
	    "reverse" => \$reverse_fragment_depgraph,
	    "transitive-closure" => \$compute_tc,
	   );

# sanity
unless (-e $fragment_depgraph_file) {
  die "The fragment dependency graph does not exist at the expected location ($fragment_depgraph_file)!";
}
unless (-r $fragment_depgraph_file) {
  die "The fragment dependency graph at '$fragment_depgraph_file' is not readable!";
}
unless (-e $item_to_fragment_table_file) {
  die "The item-to-fragment table does not exist at the expected location ($item_to_fragment_table_file)!";
}
unless (-e $item_to_fragment_table_file) {
  die "The item-to-fragment table at '$item_to_fragment_table_file' is not readable!";
}

# don't overwrite
my $tc_file = '/tmp/tc';
if ($compute_tc) {
  if (-e $tc_file) {
    print "Cannot save the transitive closure to '$tc_file' because there is already a file there", "\n";
    exit 1;
  }
}

# load the fragment dependency graph
my %fragment_depgraph = ();

sub load_fragment_depgraph {
  warn "Loading the fragment dependency graph at '$fragment_depgraph_file";
  my $num_lines = 0;
  open (FRAGMENT_DEPGRAPH, '<', $fragment_depgraph_file) 
    or die "Unable to open an input filehandle for the fragment dependency graph at '$fragment_depgraph_file': $!";
  while (defined (my $depgraph_line = <FRAGMENT_DEPGRAPH>)) {
    chomp $depgraph_line;
    $num_lines++;
    if ($num_lines % 10000 == 0) {
      warn "Seen $num_lines lines";
    }
    my ($lhs, $rhs) = split (/ /, $depgraph_line);
    unless (defined $lhs && defined $rhs) {
      die "Unable to properly parse dependeny graph line '$depgraph_line'!";
    }
    unless ($lhs ne '' && $rhs ne '') {
      die "One of the fields of the dependency graph line '$depgraph_line' is empty";
    }
    my $dep_ref = $reverse_fragment_depgraph ? $fragment_depgraph{$rhs} : $fragment_depgraph{$lhs};
    if (defined $dep_ref) {
      # warn "We've seen a reference for this reference before";
      my @earlier = @{$dep_ref};
      $reverse_fragment_depgraph ? push (@earlier, $lhs) : push (@earlier, $rhs);
      if ($reverse_fragment_depgraph) {
	$fragment_depgraph{$rhs} = \@earlier;
      } else {
	$fragment_depgraph{$lhs} = \@earlier;
      }
    } else {
      # warn "We've not seen this reference before";
      my @first = ();
      $reverse_fragment_depgraph ? push (@first, $lhs) : push (@first, $rhs);
      if ($reverse_fragment_depgraph) {
	$fragment_depgraph{$rhs} = \@first;
      } else {
	$fragment_depgraph{$lhs} = \@first;
      }
    }
  }
  close (FRAGMENT_DEPGRAPH) 
    or die "Unable to close input filehandle for the fragment dependency graph at '$fragment_depgraph_file': $!";
  warn "Done loading the fragment dependency graph at '$fragment_depgraph_file'";

  return;

}

load_fragment_depgraph ();

sub print_fragment_depgraph {
  foreach my $key (keys %fragment_depgraph) {
    my $val_ref = $fragment_depgraph{$key};
    my @vals = @{$val_ref};
    print $key;
    foreach my $val (@vals) {
      print ' ', $val;
    }
    print "\n";
  }
}

# load the item-to-fragment table
my %item_to_fragment = ();

open (ITEM_TO_FRAGMENT, '<', $item_to_fragment_table_file) 
  or die "Unable to open an input filehandle for the item-to-fragment table at '$item_to_fragment_table_file': $!";
while (defined (my $item_to_fragment_line = <ITEM_TO_FRAGMENT>)) {
  chomp $item_to_fragment_line;
  my ($lhs, $rhs) = split (/ /, $item_to_fragment_line);
  unless (defined $lhs && defined $rhs) {
    die "Unable to properly parse item-to-fragment line '$item_to_fragment_line'!";
  }
  unless ($lhs ne '' && $rhs ne '') {
    die "One of the fields of the line '$item_to_fragment_line' in the item-to-fragment table is empty";
  }
  $item_to_fragment{$lhs} = $rhs;
}
close (ITEM_TO_FRAGMENT)
  or die "Unable to close input filehandle for the item-to-fragment table at '$item_to_fragment_table_file': $!";

my %fragment_to_items = ();
while ((my $item,my $fragment) = each %item_to_fragment) {
  my $fragment = $item_to_fragment{$item};
  my $earlier_ref = $fragment_to_items{$fragment};
  if (defined $earlier_ref) {
    my @earlier = @{$earlier_ref};
    unless (grep (/$item/, @earlier)) {
      push (@earlier, $item);
      $fragment_to_items{$fragment} = \@earlier;
    }
  } else {
    # warn "this is the first item generated by fragment '$fragment'";
    my @singleton = ();
    push (@singleton, $item);
    $fragment_to_items{$fragment} = \@singleton;
  }

}

# print the fragment-to-items graph
# foreach my $key (keys %fragment_depgraph) {
#   my $val_ref = $fragment_depgraph{$key};
#   my @vals = @{$val_ref};
#   print $key;
#   foreach my $val (@vals) {
#     print ' ', $val;
#   }
#   print "\n";
# }

if ($compute_tc) {
  my $tc_file = '/tmp/tc';
  if (-e $tc_file) {
    print "Cannot save the transitive closure to '$tc_file' because there is already a file there", "\n";
    exit 1;
  }
  warn "Computing the transitive closure...";
  my $tc_exit_code
    = system ("./tc.pl $fragment_depgraph_file > $tc_file");
  if ($tc_exit_code == 0) {
    warn "Done computing transitive closure";
    # reload the fragment dependency graph
    $fragment_depgraph_file = $tc_file;
    load_fragment_depgraph ();
  } else {
    my $tc_message = $tc_exit_code >> 8;
    print "Something went wrong computing the transitive closure: '$tc_message'";
    exit 1;
  }
}

foreach my $item (keys %item_to_fragment) {
  my @deps = ();
  my $fragment = $item_to_fragment{$item};
  # warn "$item corresponds to $fragment";
  my $dep_fragments_ref = $fragment_depgraph{$fragment};
  if (defined $dep_fragments_ref) {
    my @dep_fragments = @{$dep_fragments_ref};
    foreach my $dep_fragment (@dep_fragments) {
      # warn "$item depends on fragment $dep_fragment";
      my $dep_items_ref = $fragment_to_items{$dep_fragment};
      if (defined $dep_items_ref) {
	my @dep_items = @{$dep_items_ref};
	foreach my $dep_item (@dep_items) {
	  unless (grep (/$dep_item/, @deps)) {
	    push (@deps, $dep_item);
	  }
	}
	# warn "dep items has ", scalar @dep_items, " items";
	# foreach my $dep_item (@dep_items) {
	#   print $item, ' ', $dep_item, "\n";
	# }
      } else {
	warn "the fragment '$dep_fragment' is not present in the fragment-to-items table";
	unless (grep (/$dep_fragment/, @deps)) {
	  push (@deps, $dep_fragment);
	}
      }
    }
  }
  unless (scalar @deps == 0) {
    if ($lisp_output) {
      print '(', '"', $item, '"';
    } else {
      print $item;
    }

    foreach my $dep_item (@deps) {
      if ($lisp_output) {
	print ' ', '"', $dep_item, '"';
      } else {
	print ' ', $dep_item;
      }

    }

    if ($lisp_output) {
      print ')';
    }

    print "\n";
  }

}

exit 0;
