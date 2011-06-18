#!/usr/bin/perl -w

# Extract author and title information from a BibTeX file.

use strict;

use Text::BibTeX;

unless (scalar @ARGV == 1) {
  print 'Please supply a BibTeX file as the first and only argument.', "\n";
  exit 1;
}

my $path = $ARGV[0];

# sanity
unless (-e $path) {
  print "The file '$path' doesn't exist!", "\n";
  exit 1;
}
unless (-r $path) {
  print "The file at '$path' is not readable!", "\n";
  exit 1;
}

my $bibfile = new Text::BibTeX::File $path;

while (my $entry = new Text::BibTeX::Entry $bibfile) {
  my $key = $entry->key ();
  next unless defined $key && $key =~ /\.MIZ$/;
  if ($entry->parse_ok ()) {
    my $title = $entry->get ('title');
    my @authors = $entry->get ('author');
    if (defined $title) {
      if (@authors) {
	my $key_lc = lc $key;
	$key_lc =~ s/\.abs$//;
	# print 'Key: ', $key_lc, "\n", 'Title: ', $title, "\n", 'Author(s): ', @authors, "\n";
	print '("', $key_lc, '" "', $title, '" "', @authors, '")', "\n";
	# print $key, "\n";
      } else {
	print "Warning: entry '$entry' lacks either a title", "\n";
	exit 1;
      }
    } else {
      print "Warning: entry '$entry' lacks an author", "\n";
      exit 1;
    }
  } else {
    print "Error: entry for the bib file at '$path' is unparsable", "\n";
    exit 1;
  }
}

exit 0;
