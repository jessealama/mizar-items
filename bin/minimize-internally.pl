#!/usr/bin/perl

use warnings;
use strict;
use File::Basename qw(basename dirname);
use XML::LibXML;
use POSIX qw(floor ceil);
use Getopt::Long;
use Pod::Usage;
use Carp qw(croak);

my $debug = 0;
my $paranoid = 0;
my $verbose = 0;
my $man = 0;
my $help = 0;
my $confirm_only = 0;
my $checker_only = 0;

GetOptions('help|?' => \$help,
           'man' => \$man,
           'verbose'  => \$verbose,
	   'paranoid' => \$paranoid,
	   'debug' => \$debug,
	   'checker-only' => \$checker_only,
	   'confirm-only' => \$confirm_only)
  or pod2usage(2);
pod2usage(1) if $help;
pod2usage(-exitstatus => 0, -verbose => 2) if $man;

pod2usage(1) unless (scalar @ARGV == 1);

my $article = $ARGV[0];
my $article_basename = basename ($article, '.miz');
my $article_dirname = dirname ($article);
my $article_miz = "${article_dirname}/${article_basename}.miz";
my $article_xml = "${article_dirname}/${article_basename}.xml";
my $article_err = "${article_dirname}/${article_basename}.err";

my $xml_parser = XML::LibXML->new (suppress_errors => 1,
				   suppress_warnings => 1);
my $xml_doc = undef;

if (! -e $article_xml) {
  croak ('Error: the .xml file for ', $article_basename, ' does not exist.');
}

if (! -f $article_xml) {
  croak ('Error: the .xml file for ', $article_basename, ' does not exist.');
}

if (! -r $article_xml) {
  croak ('Error: the .xml file for ', $article_basename, ' is unreadable.');
}

if (! defined eval { $xml_doc = $xml_parser->parse_file ($article_xml) } ) {
  croak 'Error: the .xml file of ', $article_basename, ' is not well-formed XML.', "\n";
}

(my $article_node) = $xml_doc->findnodes ('Article');
my $aid = aid_for_element ($article_node);

sub write_element_table {
  my @elements = @{shift ()};
  my %table = %{shift ()};
  my $path = shift;
  my $new_doc = XML::LibXML::Document->createDocument ();
  my $root = $new_doc->createElement ('Article');

  if ($debug) {
    print STDERR ('There are ', scalar @elements, ' elements available; we will now print just ', scalar (keys %table), ' of them now to ', $path, "\n");
  }

  $root->setAttribute ('aid', $aid);
  $root->appendText ("\n");
  foreach my $i (0 .. scalar @elements - 1) {
    if (defined $table{$i}) {
      my $element = $elements[$i];
      $root->appendChild ($element);
      $root->appendText ("\n");
    }
  }
  $new_doc->setDocumentElement ($root);
  $new_doc->toFile ($path);

}

sub verify {
  my $verifier_status = system ("verifier -c -q -s -l $article_miz > /dev/null 2>&1");
  my $verifier_exit_code = $verifier_status >> 8;
  if ($verifier_exit_code == 0 && -z $article_err) {
    return 1;
  } else {
    if ($debug) {
      if (-z $article_err) {
	print STDERR ('Verifier failed. (Empty err file.)', "\n");
      } else {
	print STDERR ('Verifier failed.  Contents of the err file:', "\n");
	system ("cat $article_err");
      }
    }
    return 0;
  }
}

sub render_element {
  my $element = shift;
  my @attrs = $element->attributes ();
  if (scalar @attrs == 0) {
    return '[(element without attributes)]';
  } else {
    my @sorted_attrs = sort { $a->nodeName() cmp $b->nodeName() } @attrs;
    my $rendered = '[';
    my $num_attrs = scalar @sorted_attrs;
    my $i = 0;
    foreach (my $i = 0; $i < $num_attrs; $i++) {
      my $attr = $sorted_attrs[$i];
      my $attr_name = $attr->nodeName;
      $rendered .= $attr_name;
      $rendered .= ' ==> ';
      my $val = $attr->getValue ();
      $rendered .= $val;
      if ($i < $num_attrs - 1) {
	$rendered .= ', ';
      }
    }
    $rendered .= ']';
    return $rendered;
  }
}

sub print_element {
  my $element = shift;
  print render_element ($element), "\n";
  return;
}

sub minimize {
  my @elements = @{shift ()};
  my %table = %{shift ()};
  my $path = shift;
  my $begin = shift;
  my $end = shift;

  if ($debug) {
    print 'begin = ', $begin, ' and end = ', $end, "\n";
  }

  if ($end < $begin) {
    return \%table;
  } elsif ($end == $begin) {
    # Try deleting
    delete $table{$begin};
    my $element = $elements[$begin];
    my $element_name = $element->nodeName ();
    write_element_table (\@elements, \%table, $path);
    my $deletable = verify ();
    if ($deletable == 1) {
      if ($verbose) {
	print 'We can dump element #', $begin, , ' (', $element_name, ')', "\n";
      }
    } else {
      if ($verbose) {
	print 'We cannot dump element #', $begin, ' (', $element_name, ')', "\n";
      }
      $table{$begin} = 0;
      write_element_table (\@elements, \%table, $path);
    }
    return \%table;
  } elsif ($begin + 1 == $end) {

    my $begin_element = $elements[$begin];
    my $end_element = $elements[$end];
    my $begin_element_name = $begin_element->nodeName ();
    my $end_element_name = $end_element->nodeName ();

    delete $table{$end};
    write_element_table (\@elements, \%table, $path);
    my $end_deletable = verify ();
    if ($end_deletable == 1) {
      if ($verbose) {
	print 'We can dump element #', $end, ' (', $end_element_name, ')', "\n";
      }
    } else {
      if ($verbose) {
	print 'We cannot dump element #', $end, ' (', $end_element_name, ')', "\n";
      }
      $table{$end} = 0;
      write_element_table (\@elements, \%table, $path);
    }

    delete $table{$begin};
    write_element_table (\@elements, \%table, $path);
    my $begin_deletable = verify ();

    if ($begin_deletable == 1) {
      if ($verbose) {
	print 'We can dump element #', $begin, ' (', $begin_element_name, ')', "\n";
      }
    } else {
      if ($verbose) {
	print 'We cannot dump element #', $begin, ' (', $begin_element_name, ')', "\n";
      }
      $table{$begin} = 0;
      write_element_table (\@elements, \%table, $path);
    }

    return \%table;

  } else {

    my $segment_length = $end - $begin + 1;
    my $half_segment_length = floor ($segment_length / 2);

    # Dump the upper half
    foreach my $i ($begin + $half_segment_length + 1 .. $end) {
      delete $table{$i};
    }

    # Write this to disk
    write_element_table (\@elements, \%table, $path);

    # Check that deleting the lower half is safe
    my $upper_half_safe = verify ();
    if ($upper_half_safe == 1) {
      # sleep 3;
      return (minimize (\@elements, \%table, $path, $begin, $begin + $half_segment_length));
    } else {
      # Restore the upper half
      foreach my $i ($begin + $half_segment_length + 1 .. $end) {
        $table{$i} = 0;
      }
      write_element_table (\@elements, \%table, $path);
      # Minimize just the upper half
      # sleep 3;
      my %table_for_upper_half
        = %{minimize (\@elements, \%table, $path, $begin + $half_segment_length + 1, $end)};
      # sleep 3;
      return (minimize (\@elements, \%table_for_upper_half, $path, $begin, $begin + $half_segment_length));
    }
  }
}

sub aid_for_element {
  my $element = shift;
  if ($element->exists ('@aid')) {
    return $element->findvalue ('@aid');
  } else {
    return '';
  }
}

my @elements = $xml_doc->findnodes ('Article/*');

if (scalar @elements == 0) {
  croak ('Error: the minimize-internally script makes sense only when there is a final node in the document.  But ', $article_xml, ' has no child nodes at all.');
}

my %needed = ();
foreach my $i (0 .. scalar @elements - 1) {
  $needed{$i} = 0;
}

(my $final_element) = $xml_doc->findnodes ('Article/*[position() = last()]');
my $final_element_name = $final_element->nodeName ();

my %final_needed_table;

if ($final_element_name eq 'By'
    || $final_element_name eq 'From'
    || $final_element_name eq 'Proof'
    || $final_element_name eq 'SkippedProof') {
  %final_needed_table = %{minimize (\@elements,
				    \%needed,
				    $article_xml,
				    0, scalar @elements - 3)};
  #                                    ^^^^^^^^^^^^^^^^^^^^
  #
  # We want to minimize not the final element, but the penultimate
  # element (for which the current element is the justification).
  # Thus we want to fix the final element as well as the one that
  # precedes it.
} else {
  %final_needed_table = %{minimize (\@elements,
				    \%needed,
				    $article_xml,
				    0, scalar @elements - 2)};
  #                                    ^^^^^^^^^^^^^^^^^^^^
  #
  # For all other kinds of elements, we want to preserve only the
  # final element.
}

# Check that the article is verifiable in the new minimized environment

if ($paranoid == 1) {
  my $verifier_ok = verify ();
  if ($verifier_ok == 0) {
    print 'Error: we are unable to verify ', $article_basename, ' in its newly minimized environment.', "\n";
    exit 1;
  }

  if ($verbose == 1) {
    print 'Paranoia: We have confirmed that, after minimization, ', $article_basename, ' is verifiable.', "\n";
  }

}

# Print the indices of the deleteable elements
foreach my $i (0 .. scalar @elements - 1) {
  if (! defined $final_needed_table{$i}) {
    print $i + 1, "\n";
  }
}

__END__

=cut

=head1 minimal.pl

minimal.pl - Minimize the environment of a mizar article

=head1 SYNOPSIS

minimize.pl [options] mizar-article

Options:
  -help                       Brief help message
  -man                        Full documentation
  -verbose                    Say what we're doing
  -paranoid                   Check that the article is verifiable before and after we're done minimizing
  -confirm-only               Don't minimize; simply check that the environment really is minimal

=head1 OPTIONS

=over 8

=item B<--help>

Print a brief help message and exits.

=item B<--man>

Prints the manual page and exits.

=item B<--verbose>

Say what environment file we're minimizing, and for each environment
file, say how many environment "items" are present there and how many
we really need.

=item B<--debug>

Print copious amounts of debugging information.

=item B<--paranoid>

Before minimizing, check that the article is verifiable.  If it is,
the continue, otherwise exit uncleanly.  After minimization of the
article's environment, check again that it is verifiable.  If it
isn't, then exit uncleanly.

=item B<--confirm-only>

Don't do any minimization, but check that the environment really is
minimal in the sense that there is no item from any environment file
(except the .atr file) that can be deleted while still preserving
verifiability.

=back

=head1 DESCRIPTION

B<minimize.pl> will construct, in a brute-force manner, the smallest
environment with respect to which the given article is verifiable.

At the end of internal minimization, we will print the indices
(starting from 1, not 0) of the elements that were deletable, one line
for each number.  The numbers will be sorted in numeric order.

=cut
