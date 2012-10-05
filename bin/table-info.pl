#!/usr/bin/perl

use warnings;
use strict;
use Getopt::Long;
use Pod::Usage;
use Carp qw(croak);
use File::Temp qw(tempfile);
use List::Util qw(shuffle);
use Graph;

sub ensure_readable_file {
  my $file = shift;

  if (! -e $file) {
      print {*STDERR} 'Error: ', $file, ' does not exist.', "\n";
      exit 1;
  }
  if (! -f $file) {
      print {*STDERR} 'Error: ', $file, ' is not a file.', "\n";
      exit 1;
  }

  if (! -r $file) {
      print {*STDERR} 'Error: ', $file, ' is unreadable.', "\n";
  }

  return;
}

my $opt_help = 0;
my $opt_man = 0;

GetOptions(
    'help|?' => \$opt_help,
    'man' => \$opt_man
) or pod2usage (2);

if ($opt_help) {
    pod2usage(1);
}

if ($opt_man) {
    pod2usage(-exitstatus => 0, -verbose => 2);
}

if (scalar @ARGV == 0) {
    pod2usage (1);
}

my $table_file = shift @ARGV;

ensure_readable_file ($table_file);

my @defined_items = ();
my %table = ();
my %all_items = ();
my %dep_items = ();
my %trivial_dep_items = (); # items that explicitly have no dependencies
my %non_trivial_dep_items = (); # items that have at least item on which they depend
my %used = (); # items that are used by at least one other item

sub load_table {
  open (my $table_fh, '<', $table_file)
    or croak ('Error: unable to open the dependency table file at ', $table_file, '.', "\n");
  while (defined (my $table_line = <$table_fh>)) {
    chomp $table_line;

    if ($table_line =~ / \s{2} /x) {
      croak ('Error: a line in the supplied dependency table has multiple consecutive spaces.', "\n");
    }
    if ($table_line =~ /\A \s /x) {
      croak ('Error: a line in the supplied dependency table begins with a space.', "\n");
    }
    if ($table_line =~ /\A \z/x) {
      croak ('Error: there is a blank line in the supplied dependency table.', "\n");
    }

    my ($item, my @deps) = split (/ /, $table_line);
    $dep_items{$item} = 0;
    $all_items{$item} = 0;
    if (defined $table{$item}) {
	my @original_deps = @{$table{$item}};
	push (@original_deps, @deps);
    } else {
	$table{$item} = \@deps;
    }
    foreach my $dep (@deps) {
	$all_items{$dep} = 0;
	$used{$dep} = 0;
    }
    if (scalar @deps == 0) {
	$trivial_dep_items{$item} = 0;
    } else {
	$non_trivial_dep_items{$item} = 0;
    }

    push (@defined_items, $item);

  }
  close $table_fh
    or croak ('Error: unable to close the input filehandle for ', $table_file, '.', "\n");

  return;

}

sub escape_item {
  my $item = shift;
  $item =~ s/\[/\\[/;
  $item =~ s/\]/\\]/;
  return $item;
}

my %command_dispatch_table =
  ('all' => \&all,
   'print' => \&complete_table,
   'count-all' => \&count_all,
   'dependent-items' => \&dependent_items,
   'count-dependent-items' => \&count_dependent_items,
   'explicitly-independent-items' => \&explicitly_independent_items,
   'count-explicitly-independent-items' => \&count_explicitly_independent_items,
   'implicitly-independent-items' => \&implicitly_independent_items,
   'count-implicitly-independent-items' => \&count_implicitly_independent_items,
   'independent-items' => \&independent_items,
   'count-independent-items' => \&count_independent_items,
   'dependencies-of' => \&dependencies_of,
   'users-of' => \&items_depending_on,
   'used-items' => \&used_items,
   'random-item' => \&random_item,
   'dependencies-of-random-item' => \&dependencies_of_random_item,
   'topological-sort' => \&tsort,
   'items-independent-of' => \&items_independent_of,
   'used' => \&used,
   'unused' => \&unused,
   'sort' => \&sort_table,
   'shuffle' => \&shuffle_table,
   'complete' => \&complete_table,
   'invert' => \&invert_table,
   'structural-check' => \&check_structurally,
   'predecessors' => \&predecessors_of_item,
   'successors' => \&successors_of_item,
   'truncate' => \&truncate_at_items,
   'inverse-truncate' => \&inverse_truncate_at_items,
   'generated-subgraph' => \&generated_subgraph,
   'promote-to-axiom' => \&promote_to_axiom,
   'mizar-tests' => \&run_mizar_tests);

if (scalar @ARGV == 0) {
    pod2usage (1);
}

my $command = $ARGV[0];
chomp $command;

if (! defined $command_dispatch_table{$command}) {
  pod2usage (1);
}

# Command line arguments seem to be valid; let's load the table.

load_table ();

sub all {
  print join ("\n", keys %all_items), "\n";
}

sub count_all {
  print scalar keys %all_items, "\n";
}

sub dependent_items {
  if (scalar keys %dep_items > 0) {
    print join ("\n", keys %dep_items), "\n";
  }
}

sub count_dependent_items {
  print scalar keys %dep_items, "\n";
}

sub get_implicitly_independent_items {
    my @items = ();
    foreach my $item (keys %all_items) {
	if (! defined $dep_items{$item}) {
	    push (@items, $item);
	}
    }

    if (wantarray) {
	return @items;
    } else {
	return \@items;
    }
}

sub implicitly_independent_items {
    my @items = get_implicitly_independent_items ();
    foreach my $item (@items) {
	print $item, "\n";
    }
}

sub count_implicitly_independent_items {
  my $c = 0;
  foreach my $item (keys %all_items) {
    if (! defined $dep_items{$item}) {
      $c++;
    }
  }
  print $c, "\n";
}

sub explicitly_independent_items {
  if (scalar keys %trivial_dep_items > 0) {
    print join ("\n", keys %trivial_dep_items), "\n";
  }
}

sub count_explicitly_independent_items {
  print scalar keys %trivial_dep_items, "\n";
}

sub independent_items {
    my @trivial_deps = keys %trivial_dep_items;
    my @implicitly_independent = get_implicitly_independent_items ();
    foreach my $item (@trivial_deps, @implicitly_independent) {
	print $item, "\n";
    }
}

sub count_independent_items {

  my $c = 0;
  foreach my $item (keys %all_items) {
    if (! defined $dep_items{$item}) {
      $c++;
    }
  }

  print $c + scalar keys %trivial_dep_items, "\n";
}

sub dependencies_of {
  if (scalar @ARGV != 2) {
    pod2usage (1);
  }
  my $item = $ARGV[1];
  if (defined $table{$item}) {
    my @deps = @{$table{$item}};
    if (@deps) {
      print join ("\n", @deps), "\n";
    }
  }
}

sub used_items {
  my %seen = ();
  foreach my $item (keys %all_items) {
    my $item_escaped = escape_item ($item);
    foreach my $other_item (keys %non_trivial_dep_items) {
      my @deps = @{$table{$other_item}};
      if (grep { /\A $item_escaped \z/x } @deps) {
	if (defined $seen{$item}) {
	  # don't do anything: we've seen $item before
	} else {
	  print $item, "\n";
	  $seen{$item} = 0;
	}
      }
    }
  }
}

sub items_depending_on {
  if (scalar @ARGV != 2) {
    pod2usage (1);
  }
  my $item = $ARGV[1];
  my $item_escaped = escape_item ($item);
  foreach my $other_item (keys %dep_items) {
    my @deps = @{$table{$other_item}};
    if (grep { / \A $item_escaped \z/x } @deps) {
      print $other_item, "\n";
    }
  }
}

sub random_item {
  my @items = keys %all_items;
  my $random_number = rand scalar @items;
  my $random_item = $items[$random_number];
  print $random_item, "\n";
}

sub dependencies_of_random_item {
  my @items_with_deps = keys %non_trivial_dep_items;
  if (scalar @items_with_deps == 0) {
    croak ('Error: there are no items that have a non-empty list of dependencies.', "\n");
  } else {
    my $random_number = rand scalar @items_with_deps;
    my $random_item = $items_with_deps[$random_number];
    if (defined $table{$random_item}) {
      my @deps = @{$table{$random_item}};
      print join ("\n", @deps), "\n";
    } else {
      croak ('Error: we computed ', $random_item, ' as a random item, but somehow we cannot find this item in our dependency table.');
    }
  }
}

sub tsort {
  my $split_vertices_fh = File::Temp->new ();
  my $split_vertices_path = $split_vertices_fh->filename ();
  foreach my $item (keys %dep_items) {
    my @deps = @{$table{$item}};
    foreach my $dep (@deps) {
      print {$split_vertices_fh} $item, ' ', $dep, "\n";
    }
  }
  close $split_vertices_fh
    or croak ('Error: unable to close the output filehandle for the temporary file created to reformat ', $table_file, ' so that we may call tsort.', "\n");
  my $tsort_error_fh = File::Temp->new ();
  my $tsort_output_fh = File::Temp->new ();
  my $tsort_error_path = $tsort_error_fh->filename ();
  my $tsort_output_path = $tsort_output_fh->filename ();
  my $tsort_status
    = system ("tsort $split_vertices_path > $tsort_output_path 2> $tsort_error_path");
  my $tsort_exit_code = $tsort_status >> 8;
  if ($tsort_exit_code != 0) {
    croak ('Error: tsort did not exit cleanly.', "\n");
  }
  if (-s $tsort_error_path) {
    my @tsort_errors = `cat $tsort_error_path`;
    croak ('Error: although tsort exited cleanly, it did report some errors:', "\n", join ("\n", @tsort_errors), "\n");
  }
  system ("cat $tsort_output_path");

  # Now print the items that appear in the table but don't have known
  # dependencies, as well as the items that have zero known
  # dependencies.

  foreach my $item (keys %all_items) {
    if (defined $table{$item}) {
      my @deps = @{$table{$item}};
      if (@deps == 0) {
	print $item, "\n";
      }
    } else {
      print $item, "\n";
    }
  }

}

sub items_independent_of {
  if (scalar @ARGV != 2) {
    pod2usage (1);
  }
  my $item = $ARGV[1];
  my $item_escaped = escape_item ($item);
  foreach my $other_item (keys %all_items) {
    # Does $other_item have any dependencies at all
    if (defined $table{$other_item}) {
      my @deps = @{$table{$other_item}};
      # Does $other_item have an empty list of dependencies?
      if (scalar @deps == 0) {
	# If so, then it is independent of $item
      } else {
	if (grep { /\A $item_escaped \z/x } @deps) {
	  # $other_item does depend on $item, so don't print anything
	} else {
	  # We didn't find $item among the dependencies of $other_item
	  print $other_item, "\n";
	}
      }
    } else {
      # If not, then it is independent of $item
      print $other_item, "\n";
    }
  }
}

# Items that are not used by any other item
sub unused {
  my @items_with_non_trivial_deps = keys %non_trivial_dep_items;
  my $num_items_with_non_trivial_deps = scalar @items_with_non_trivial_deps;
  foreach my $item (keys %all_items) {
    my $item_escaped = $item;
    $item_escaped =~ s/\[/\\[/;
    $item_escaped =~ s/\]/\\]/;
    my $user_found = 0;
    my $i = 0;
    while ($i < $num_items_with_non_trivial_deps && ! $user_found) {
      my $other_item = $items_with_non_trivial_deps[$i];
      my @deps = @{$table{$other_item}};
      if (grep { /\A $item_escaped \z /x } @deps) {
	$user_found = 1;
      } else {
	$i++;
      }
    }
    if (! $user_found) {
      print $item, "\n";
    }
  }
}

sub used {
  if (scalar keys %used > 0) {
    print join ("\n", keys %used), "\n";
  }
}

sub sort_table {
  foreach my $item (sort keys %all_items) {
    print $item;
    if (defined $table{$item}) {
      my @deps = @{$table{$item}};
      if (scalar @deps > 0) {
	print ' ', join (' ', sort @deps);
      }
    }
    print "\n";
  }
}

sub shuffle_table {
  my @items = keys %all_items;
  my @shuffled_items = shuffle (@items);
  foreach my $item (@shuffled_items) {
    print $item;
    if (defined $table{$item}) {
      my @deps = @{$table{$item}};
      my @shuffled_deps = shuffle (@deps);
      print ' ', join (' ', @shuffled_deps);
    }
    print "\n";
  }
}

sub complete_table {
  foreach my $item (keys %all_items) {
    print $item;
    if (defined $table{$item}) {
      my @deps = @{$table{$item}};
      if (@deps) {
	print ' ', join (' ', @deps);
      }
    }
    print "\n";
  }
}

sub invert_table {

  my %inverted = ();

  foreach my $item (keys %all_items) {
    if (defined $table{$item}) {
      my @deps = @{$table{$item}};
      foreach my $dep (@deps) {
	if (defined $inverted{$dep}) {
	  my @inverse_deps = @{$inverted{$dep}};
	  push (@inverse_deps, $item);
	  $inverted{$dep} = \@inverse_deps;
	} else {
	  my @first_inversion = ();
	  push (@first_inversion, $item);
	  $inverted{$dep} = \@first_inversion;
	}
      }
    }
  }

  foreach my $item (keys %all_items) {
    print $item;
    if (defined $inverted{$item}) {
      my @inverse_deps = @{$inverted{$item}};
      print ' ', join (' ', @inverse_deps);
    }
    print "\n";
  }

}

sub load_graph {

  my $g = Graph->new ();

  foreach my $item (keys %all_items) {
    $g->add_vertex ($item);
  }

  foreach my $item (keys %non_trivial_dep_items) {
    my @deps = @{$table{$item}};
    foreach my $dep (@deps) {
      $g->add_edge ($dep, $item); # orient things this way to align
                                  # our notion of dependence with the
                                  # Graph module's notion of successor
                                  # and predecessor.
    }
  }

  return $g;

}

sub check_structurally {

    # check for self dependency
    foreach my $item (keys %table) {
	my @deps = @{$table{$item}};
	foreach my $dep (@deps) {
	    if ($dep eq $item) {
		print $item, ' depends on itself', "\n";
	    }
	}
    }

    my %encountered = ();
    foreach my $item (@defined_items) {
	if (defined $encountered{$item}) {
	    print $item, ' occurs as the first item of a dependency line more than once.', "\n";
	    exit 1;
	}
	if (defined $table{$item}) {
	    my @deps = @{$table{$item}};
	    foreach my $dep_item (@deps) {
		if (! defined $encountered{$dep_item}) {
		    if (defined $table{$dep_item}) {
			print $item, ' depends on ', $dep_item, ', whose dependencies are not yet known.', "\n";
			exit 1;
		    }
		}
	    }
	}
	$encountered{$item} = 0;
    }

    my $g = load_graph ();

    my @cycle = $g->find_a_cycle ();

    if (scalar @cycle > 0) {
	my $cycle_length = scalar @cycle;
	print $table_file, ' has at least one cycle of length ', $cycle_length, ':', "\n", join ("\n", @cycle), "\n";
	print '(There may be more cycles; we are printing only one.)', "\n";
    } else {
	print $table_file, ' is acyclic.', "\n";
    }
}

sub predecessors_of_item {

  if (scalar @ARGV != 2) {
    pod2usage (1);
  }
  my $item = $ARGV[1];

  if (! defined $all_items{$item}) {
    croak ('Error: ', $item, ' is not present in ', $table_file, '.', "\n");
  }

  if (defined $non_trivial_dep_items{$item}) {
    my $g = load_graph ();

    my @preds = $g->all_predecessors ($item);

    if (scalar @preds > 0) {
      print join ("\n", @preds), "\n";
    }
  }

}

sub successors_of_item {

  if (scalar @ARGV != 2) {
    pod2usage (1);
  }
  my $item = $ARGV[1];

  if (! defined $all_items{$item}) {
    croak ('Error: ', $item, ' is not present in ', $table_file, '.', "\n");
  }

  my $g = load_graph ();

  my @succs = $g->all_successors ($item);

  if (scalar @succs > 0) {
    print join ("\n", @succs), "\n";
  }

}

sub truncate_at_items {

  if (scalar @ARGV < 2) {
    pod2usage (1);
  }

  my @items = @ARGV[1 .. scalar @ARGV - 1];

  foreach my $item (@items) {
    if (! defined $all_items{$item}) {
      croak ('Error: ', $item, ' is not present in ', $table_file, '.', "\n");
    }
  }

  my $g = load_graph ();

  my @preds = $g->all_predecessors (@items);

  my %pred_table = ();
  foreach my $item (@items, @preds) {
    $pred_table{$item} = 0;
  }

  my %handled = (); # to avoid printing items and their dependencies
                    # multiple times

  foreach my $item (@items, @preds) {
    if (defined $handled{$item}) {
      # we've seen this before; don't do anything
    } else {
      print $item;
      if (defined $non_trivial_dep_items{$item}) {
	my @deps = @{$table{$item}};
	foreach my $dep (@deps) {
	  if (defined $pred_table{$dep}) {
	    print ' ', $dep;
	  } else {
	    # don't print $dep: there is no path from any of @items to it
	  }
	}
      }
      print "\n";

      $handled{$item} = 0;

    }
  }

}

sub inverse_truncate_at_items {

  if (scalar @ARGV < 2) {
    pod2usage (1);
  }

  my @items = @ARGV[1 .. scalar @ARGV - 1];

  foreach my $item (@items) {
    if (! defined $all_items{$item}) {
      croak ('Error: ', $item, ' is not present in ', $table_file, '.', "\n");
    }
  }

  my $g = load_graph ();

  my @succs = $g->all_successors (@items);

  my %succ_table = ();
  foreach my $item (@items, @succs) {
    $succ_table{$item} = 0;
  }

  my %handled = (); # to avoid printing items and their dependencies
                    # multiple times

  foreach my $item (@items, @succs) {
    if (defined $handled{$item}) {
      # we've seen this before; don't do anything
    } else {

      print $item;
      if (defined $non_trivial_dep_items{$item}) {
	my @deps = @{$table{$item}};
	foreach my $dep (@deps) {
	  if (defined $succ_table{$dep}) {
	    print ' ', $dep;
	  } else {
	    # don't print $dep: there is no path from it to any of @items
	  }
	}
      }
      print "\n";

      $handled{$item} = 0;

    }
  }

}

sub generated_subgraph {

  if (scalar @ARGV < 2) {
    pod2usage (1);
  }

  my @items = @ARGV[1 .. scalar @ARGV - 1];

  foreach my $item (@items) {
    if (! defined $all_items{$item}) {
      croak ('Error: ', $item, ' is not present in ', $table_file, '.', "\n");
    }
  }

  my $g = load_graph ();

  my @preds = $g->all_predecessors (@items);
  my @succs = $g->all_successors (@items);

  my %generated = ();
  foreach my $item (@items, @preds, @succs) {
    $generated{$item} = 0;
  }

  my %handled = (); # to avoid printing items and their dependencies
                    # multiple times

  foreach my $item (@items, @preds, @succs) {
    if (defined $handled{$item}) {
      # we've seen this before; don't do anything
    } else {
      print $item;
      if (defined $non_trivial_dep_items{$item}) {
	my @deps = @{$table{$item}};
	foreach my $dep (@deps) {
	  if (defined $generated{$dep}) {
	    print ' ', $dep;
	  } else {
	    # don't print $dep: there is no path to it from it or to
	    # that passes through a member of @items
	  }
	}
      }
      print "\n";

      $handled{$item} = 0;

    }
  }

}

sub promote_to_axiom {

  if (scalar @ARGV < 2) {
    pod2usage (1);
  }

  my @items = @ARGV[1 .. scalar @ARGV - 1];

  my %items_table = ();
  foreach my $item (@items) {
    if (! defined $all_items{$item}) {
      croak ('Error: ', $item, ' is not present in ', $table_file, '.', "\n");
    }
    $items_table{$item} = 0;
  }

  my $g = load_graph ();

  # Find the "frontier" items: the items that are not used by any
  # other item
  my %frontier = ();
  foreach my $item ($g->vertices ()) {
    if ($g->out_degree ($item) == 0) {
      $frontier{$item} = 0;
    }
  }

  my %handled = ();
  my @envelope = keys %frontier;
  while (@envelope) {
    my $item = pop @envelope;
    if (defined $handled{$item}) {
      # we've handled this guy -- nothing to do
    } else {

      print $item;

      if (defined $items_table{$item}) {
	# stop: we've reached the new axiom
      } else {
	if (defined $non_trivial_dep_items{$item}) {
	  my @deps = @{$table{$item}};
	  foreach my $dep (@deps) {
	    print ' ', $dep;
	    push (@envelope, $dep);
	  }
	}
      }

      print "\n";

      $handled{$item} = 0;

    }
  }

}

sub is_constructor_item {
  my $item = shift;
  if (defined $item) {
    if ($item =~ / \A [a-z0-9_]+ : . constructor : [0-9]+ \z /x) {
      return 1;
    } else {
      return 0;
    }
  } else {
    croak ('Error: we cannot determine whether an undefined value is a constructor item.', "\n");
  }
}

sub is_pattern_item {
  my $item = shift;
  if (defined $item) {
    if ($item =~ / \A [a-z0-9_]+ : . pattern : [0-9]+ \z /x) {
      return 1;
    } else {
      return 0;
    }
  } else {
    croak ('Error: we cannot determine whether an undefined value is a pattern item.', "\n");
  }
}

sub run_mizar_tests {

  # Patterns depend only on patterns and constructors
  print 'Does every pattern depend only upon constructors? ';
  my $already_said_no = 0;
  foreach my $item (keys %non_trivial_dep_items) {
    if (is_pattern_item ($item)) {
      my @deps = @{$table{$item}};
      foreach my $dep (@deps) {
	if (! is_constructor_item ($dep)) {
	  if ($already_said_no) {
	    print '* ', $item, ' depends on ', $dep, "\n";
	  } else {
	    print 'No:', "\n";
	    $already_said_no = 1;
	  }
	}
      }
    }
  }

  if (! $already_said_no) {
    print 'Yes.', "\n";
  }

}

&{$command_dispatch_table{$command}};


__END__

=pod

=head1 NAME

B<table-info.pl>: Print information about a dependency table

=head1 SYNOPSIS

B<table-info.pl>: <dependency-table> <command> [arguments]

=head1 OPTIONS

=over 8

=back

=head

=cut
