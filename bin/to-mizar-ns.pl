#!/usr/bin/perl -w

use strict;

# Map items expressed in the the mizar-items namespace convention to
# the canonical mizar namespace

sub to_mizar_ns {
  my $article = shift;
  my $kind = shift;
  my $number = shift;
  if ($kind eq 'lconstructor') {
    return ('existence' . '_' . 'l' . $number . '_' . $article);
  }
  if ($kind eq 'mconstructor') {
    return ('existence' . '_' . 'm' . $number . '_' . $article);
  }
  if ($kind eq 'theorem') {
    return ('t' . $number . '_' . $article);
  }
  if ($kind eq 'lemma') {
    return ('l' . $number . '_' . $article);
  }
  if ($kind eq 'scheme') {
    return ('s' . $number . '_' . $article);
  }
  if ($kind eq 'ccluster') {
    return ('cc' . $number . '_' . $article);
  }
  if ($kind eq 'fcluster') {
    return ('fc' . $number . '_' . $article);
  }
  if ($kind eq 'rcluster') {
    return ('rc' . $number . '_' . $article);
  }
  if ($kind eq 'deftheorem') {
    return ('dt' . $number . '_' . $article);
  }
  if ($kind eq 'definiens') {
    return ('dt' . $number . '_' . $article);
  }
  if ($kind eq 'mpattern') {
    return ''; # doesn't exist in Josef's namespace
  }
  return '';
}

my $some_invalid_item = 0;
my $some_unresolved_item = 0;

while (defined (my $item_line = <STDIN>)) {
  chomp $item_line;
  $item_line =~ s/ \t//g; # kill space and tabs
  if ($item_line =~ /:/) {
    (my $article, my $kind, my $number) = split ':', $item_line;
    # don't try to resolve items that we know don't exist in Josef's
    # namespace
    if ($kind eq 'mpattern'
	or $kind eq 'rpattern'
	or $kind eq 'kpattern'
        or $kind eq 'upattern'
	or $kind eq 'vpattern'
	or $kind eq 'jpattern'
	or $kind eq 'gconstructor'
        or $kind eq 'vconstructor'
        or $kind eq 'rconstructor'
        or $kind eq 'kconstructor'
        or $kind eq 'uconstructor') {
      # do nothing
    } else {
      my $mapped_name = to_mizar_ns ($article, $kind, $number);
      if ($mapped_name eq '') {
	warn "Unable to resolve the mizar-items name '$item_line'";
	$some_unresolved_item = 1;
      } else {
	print $mapped_name, "\n";
      }
    }
  } else {
    warn "The item '$item_line' is not the name of a mizar-items item, because it lacks a colon";
    $some_invalid_item = 1;
  }
}

if ($some_unresolved_item == 1) {
  if ($some_invalid_item == 1) {
    exit 3;
  } else {
    exit 2;
  }
} else {
  if ($some_invalid_item == 1) {
    exit 1;
  } else {
    exit 0;
  }
}
