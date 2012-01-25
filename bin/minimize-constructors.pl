#!/usr/bin/perl -w

use strict;
use File::Basename qw(basename dirname);
use File::Copy;
use POSIX qw(ceil floor);

if (scalar @ARGV != 1) {
  print 'Usage: minimize-constructors.pl ARTICLE', "\n";
  exit 1;
}

my $article = $ARGV[0];
my $article_basename = basename ($article, '.miz');
my $article_miz = "${article_basename}.miz";
my $article_dirname = dirname ($article);
my $article_atr = "$article_dirname/${article_basename}.atr";
my $article_atr_saved = "$article_atr.saved";
my $article_atr_temp = "$article_atr.temp";

unless (-e $article_miz) {
  print 'Error: the specified article, ', $article, ', does not exist.', "\n";
  exit 1;
}

if (-d $article_miz) {
  print 'Error: the specified article, ', $article, ', is actually a directory.', "\n";
  exit 1;
}

unless (-r $article_miz) {
  print 'Error: the specified article, ', $article, ', is unreadable.', "\n";
  exit 1;
}

# Configuration

my $constructors_stylesheet = '/Users/alama/sources/mizar/xsl4mizar/items/promotable-constructors.xsl';
my $promote_constructors_stylesheet = '/Users/alama/sources/mizar/xsl4mizar/items/promote.xsl';

unless (-e $constructors_stylesheet) {
  print 'Error: we expected to find the constructors stylesheet at ', $constructors_stylesheet, ', but there is no file there.', "\n";
  exit 1;
}

unless (-r $constructors_stylesheet) {
  print 'Error: the constructors stylesheet at ', $constructors_stylesheet, ', is not readable.', "\n";
  exit 1;
}

unless (-e $promote_constructors_stylesheet) {
  print 'Error: we expected to find the constructor promotion stylesheet at ', $promote_constructors_stylesheet, ', but there is no file there.', "\n";
  exit 1;
}

unless (-r $promote_constructors_stylesheet) {
  print 'Error: the constructors promotion stylesheet at ', $promote_constructors_stylesheet, ', is not readable.', "\n";
  exit 1;
}

unless (-e $article_atr) {
  print 'Error: the .atr file for ', $article, ' does not exist.', "\n";
  exit 1;
}

unless (-r $article_atr) {
  print 'Error: the .atr file for ', $article, ' is not readable.', "\n";
  exit 1;
}

sub chunkify {
  my @list = @{shift ()};
  my $len = scalar @list;
  my $sqrt_len = sqrt $len;
  my $sqrt_len_floor = floor ($sqrt_len);

  # DEBUG
  # print 'Floor of the sqrt of ', $len, ' = ', $sqrt_len_floor, "\n";
  my @chunks = ();
  if (scalar @list > 0) {
    foreach my $i (0 .. $sqrt_len_floor) {
      my @chunk;
      if ($i == $sqrt_len_floor) {
        @chunk = @list[$sqrt_len_floor * $i .. $len - 1];
        # DEBUG
        # print 'This chunk has ', scalar @chunk, ' elements.';
      } else {
        @chunk = @list[$sqrt_len_floor * $i .. ($sqrt_len_floor * ($i + 1)) - 1];
        # DEBUG
        # print 'This chunk has ', scalar @chunk, ' elements.';
      }

      push (@chunks, \@chunk);
    }
  }
  return \@chunks;
}

my %promotable_constructors_table = ();

sub try_removing_constructor {
  my $constructor = shift;

  # Save the state of the .atr
  copy ($article_atr, $article_atr_saved);

  # Construct the XSLT parameter
  my $xslt_param = '';
  foreach my $c (keys %promotable_constructors_table) {
    $xslt_param .= ',' . $c;
  }
  $xslt_param .= ',' . $constructor . ',';

  # Write the new .atr
  my $write_new_atr_status = system ("xsltproc --stringparam constructors $xslt_param $promote_constructors_stylesheet $article_atr > $article_atr_temp");
  my $write_new_atr_exit_code = $write_new_atr_status >> 8;
  if ($write_new_atr_exit_code == 0) {
    move ($article_atr_temp, $article_atr);
  } else {
    print 'Error: we failed to construct a new .atr for ', $article, '.', "\n";
    exit 1;
  }

  # Test whether the new .atr works
  my $verifier_status = system ("verifier -q -s -l $article > /dev/null 2>&1");
  my $verifier_exit_code = $verifier_status >> 8;
  if ($verifier_exit_code == 0) {
    # DEBUG
    # warn 'We can dump ', $constructor;
    unlink $article_atr_saved;
    return 1;
  } else {
    # DEBUG
    # warn 'We cannot dump ', $constructor, ' (exit code is ', $verifier_exit_code, ')';
    move ($article_atr_saved, $article_atr);
    return 0;
  }
}

my @initial_constructors = `xsltproc $constructors_stylesheet $article_atr 2> /dev/null`;
my @initial_unpromotable_consructors = `xsltproc --stringparam kind 'unpromotable' $constructors_stylesheet $article_atr 2>/dev/null`;
chomp @initial_constructors;
chomp @initial_unpromotable_consructors;

# DEBUG
#print 'There are ', scalar @initial_constructors, ' initial constructors:';
#foreach my $c (@initial_constructors) {
#  print $c, "\n";
#}

my @initial_chunks = @{chunkify (\@initial_constructors)};
my %promotable_chunks_table = ();

# DEBUG
#print 'There are ', scalar @initial_chunks, ' initial chunks:', "\n";
# foreach my $i (1 .. scalar @initial_chunks) {
#   my $chunk_ref = $initial_chunks[$i - 1];
#   my @chunk = @{$chunk_ref};
#   print $i, ':';
#   foreach my $item (@chunk) {
#     print ' ', $item;
#   }
#   print "\n";
# }

sub try_promoting_chunk {
  my @chunk = @{shift ()};

  # Save the state of the .atr
  copy ($article_atr, $article_atr_saved);

  # Construct the XSLT parameter
  my $xslt_param = '';
  foreach my $other_chunk_index (keys %promotable_chunks_table) {
    my @other_chunk = @{$initial_chunks[$other_chunk_index]};
    foreach my $c (@other_chunk) {
      $xslt_param .= ',' . $c;
    }
  }
  foreach my $c (@chunk) {
    $xslt_param .= ',' . $c;
  }
  $xslt_param .= ',';

  # Write the new .atr
  my $write_new_atr_status = system ("xsltproc --stringparam constructors '$xslt_param' $promote_constructors_stylesheet $article_atr > $article_atr_temp");
  my $write_new_atr_exit_code = $write_new_atr_status >> 8;
  if ($write_new_atr_exit_code == 0) {
    move ($article_atr_temp, $article_atr);
  } else {
    print 'Error: we failed to construct a new .atr for ', $article, '.', "\n";
    exit 1;
  }

  # Test whether the new .atr works
  my $verifier_status = system ("verifier -q -s -l $article > /dev/null 2>&1");
  my $verifier_exit_code = $verifier_status >> 8;
  if ($verifier_exit_code == 0) {
    # DEBUG
    # warn 'We can dump ', $constructor;
    unlink $article_atr_saved;
    return 1;
  } else {
    # DEBUG
    # warn 'We cannot dump ', $constructor, ' (exit code is ', $verifier_exit_code, ')';
    move ($article_atr_saved, $article_atr);
    return 0;
  }
}

foreach my $i (1 .. scalar @initial_chunks) {
  my @chunk = @{$initial_chunks[$i-1]};
  my $promotable = try_promoting_chunk (\@chunk);
  if ($promotable == 1) {
    $promotable_chunks_table{$i - 1} = 0;
  }
}

# We now know which chunks are wholesale promotable.  Pre-populate the
# promotable constructors table with this information.

foreach my $chunk_index (keys %promotable_chunks_table) {
  my @chunk = @{$initial_chunks[$chunk_index]};
  foreach my $c (@chunk) {
    $promotable_constructors_table{$c} = 0;
  }
}

# Construct a list of constructors to inspect, i.e., constructors
# belonging to chunks that we couldn't wholesale remove.

my @maybe_promotable_constructors = ();
foreach my $chunk_index (0 .. scalar @initial_chunks - 1) {
  unless (defined $promotable_chunks_table{$chunk_index}) {
    my @chunk = @{$initial_chunks[$chunk_index]};
    push (@maybe_promotable_constructors, @chunk);
  }
}

foreach my $constructor (@maybe_promotable_constructors) {
  my $promotable = try_removing_constructor ($constructor);
  if ($promotable == 1) {
    $promotable_constructors_table{$constructor} = 0;
  }
}

# Check whether our analyzer heuristic worked: does the whole article
# pass the (full) verifier)?

# DEBUG
#warn 'Done minimizing, using the analyzer.  Testing whether the article is verifiable...';

# # Construct the XSLT parameter
# my $xslt_param = '';
# foreach my $c (keys %promotable_constructors_table) {
#   $xslt_param .= ',' . $c;
# }
# unless ($xslt_param eq '') {
#   $xslt_param .= ',';
# }

# # Write the new .atr
# my $write_new_atr_status = system ("xsltproc --stringparam constructors '$xslt_param' $promote_constructors_stylesheet $article_atr > $article_atr_temp");
# my $write_new_atr_exit_code = $write_new_atr_status >> 8;
# if ($write_new_atr_exit_code == 0) {
#   move ($article_atr_temp, $article_atr);
# } else {
#   print 'Error: we failed to construct a new .atr for ', $article, '.', "\n";
#   exit 1;
# }

# my $verifier_status = system ("verifier -q -s -l $article > /dev/null 2>&1");
# my $verifier_exit_code = $verifier_status >> 8;
# if ($verifier_exit_code == 0) {
#   # DEBUG
#   warn 'Heuristic succeeded!';
# } else {
#   # DEBUG
#   warn 'Heuristic failed!';
# }

# foreach my $constructor (keys %promotable_constructors_table) {
#   print $constructor, "\n";
# }

foreach my $constructor (@initial_unpromotable_consructors) {
  print $constructor, "\n";
}

foreach my $constructor (@initial_constructors) {
  unless (defined $promotable_constructors_table{$constructor}) {
    print $constructor, "\n";
  }
}
