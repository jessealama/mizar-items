#!/usr/bin/perl

use warnings;
use strict;
use File::Basename qw(basename dirname);
use XML::LibXML;
use POSIX qw(floor ceil);
use Getopt::Long;
use Pod::Usage;
use Carp qw(croak);
use IPC::Run qw(run);

use FindBin;
use lib "$FindBin::Bin/../lib";

use Utils qw(ensure_directory ensure_readable_file ensure_executable);
use Mizar qw(verifier);

my $paranoid = 0;
my $verbose = 0;
my $debug = 0;
my $man = 0;
my $help = 0;
my $confirm_only = 0;
my $checker_only = 0;
my $script_home = '/Users/alama/sources/mizar/xsl4mizar/items';
my $fast_theorems = 0;
my $fast_schemes = 0;

GetOptions('help|?' => \$help,
           'man' => \$man,
           'verbose'  => \$verbose,
	   'debug' => \$debug,
	   'paranoid' => \$paranoid,
	   'fast-schemes' => \$fast_schemes,
	   'script-home' => \$script_home,
	   'fast-theorems' => \$fast_theorems,
	   'checker-only' => \$checker_only,
	   'confirm-only' => \$confirm_only)
  or pod2usage(2);
pod2usage(1) if $help;
pod2usage(-exitstatus => 0, -verbose => 2) if $man;

pod2usage(1) unless (scalar @ARGV == 1);

ensure_directory ($script_home);

my $minimize_properties_script = "${script_home}/minimize-properties.pl";
my $minimize_abstractness_script = "${script_home}/minimize-abstractness.pl";
ensure_executable ($minimize_properties_script);
ensure_executable ($minimize_abstractness_script);

my $article = $ARGV[0];
my $article_basename = basename ($article, '.miz');
my $article_dirname = dirname ($article);
my $article_sans_extension = "${article_dirname}/${article_basename}";

sub article_file_with_extension {
    my $extension = shift;
    return "${article_dirname}/${article_basename}.${extension}";
}

my $article_miz = article_file_with_extension ('miz');
my $article_err = article_file_with_extension ('err');
my $article_eno = article_file_with_extension ('eno');;
my $article_refx = article_file_with_extension ('refx');;
my $article_esh = article_file_with_extension ('esh');;
my $article_eth = article_file_with_extension ('eth');;
my $article_xml = article_file_with_extension ('xml');;

my @extensions_to_minimize = ('eno', 'erd', 'epr', 'dfs', 'eid', 'ecl');
my %extension_to_element_table = ('eno' => 'Notations',
                                  'erd' => 'ReductionRegistrations',
                                  'epr' => 'PropertyRegistration',
                                  'dfs' => 'Definientia',
                                  'eid' => 'IdentifyRegistrations',
                                  'ecl' => 'Registrations',
				  'esh' => 'Schemes',
				  'eth' => 'Theorems');

my $xml_parser = XML::LibXML->new (suppress_errors => 1,
				   suppress_warnings => 1);
my $xml_doc = undef;

if (! defined eval { $xml_doc = $xml_parser->parse_file ($article_eno) } ) {
    croak ('Error: the .eno file of ', $article_basename, ' is not well-formed XML.');
}

my $aid;
if ($xml_doc->exists ('/Notations[@aid]')) {
  $aid = $xml_doc->findvalue ('/Notations/@aid')
} else {
  print 'Error: the patterns file at ', $article_eno, ' does not have a root Notations element with an aid attribute.', "\n";
  exit 1;
}

sub write_element_table {
  my @elements = @{shift ()};
  my %table = %{shift ()};
  my $path = shift;
  my $root_element_name = shift;
  my $new_doc = XML::LibXML::Document->createDocument ();
  my $root = $new_doc->createElement ($root_element_name);

  # DEBUG
  # warn 'There are ', scalar @elements, ' available; we will now print just ', scalar (keys %table), ' of them now to ', $path;
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

sub our_verify {
    my %verifier_parameters =
	(
	    'quiet' => 1,
	    'stop-on-error' => 1,
	    'long-lines' => 1,
	    'checker-only' => $checker_only,
	);
    return verify ($article_miz, \%verifier_parameters);
}

sub prune_theorems {
  my $refx_doc = undef;
  eval {
    $refx_doc = $xml_parser->parse_file ($article_refx);
  };
  if ($@) {
    print 'Error: the .refx file of ', $article_basename, ' is not well-formed XML.', "\n";
    exit 1;
  }

  my %theorems = ();
  my %definitions = ();

  my @middle_sy_three_dots = $refx_doc->findnodes ('Parser/syThreeDots[preceding-sibling::sySemiColon and following-sibling::sySemiColon[2]]');
  # DEBUG
  # print 'In the middle sySemiColon segment, there are ', scalar @middle_sy_three_dots, ' syThreeDots elements.', "\n";
  foreach my $sy_three_dots (@middle_sy_three_dots) {
    my $aid = $sy_three_dots->findvalue ('following-sibling::Int[1]/@x');
    my $nr = $sy_three_dots->findvalue ('following-sibling::Int[2]/@x');
    unless (defined $aid && defined $nr) {
      print 'Error: we failed to extract either the first or the second Int following an syThreeDots element!', "\n";
      exit 1;
    }
    $theorems{"$aid:$nr"} = 0;
  }
  my @final_sy_three_dots = $refx_doc->findnodes ('Parser/syThreeDots[preceding-sibling::sySemiColon and following-sibling::sySemiColon[1] and not(following-sibling::sySemiColon[2])]');
  # DEBUG
  # print 'In the final sySemiColon segment, there are ', scalar @final_sy_three_dots, ' syThreeDots elements.', "\n";
  foreach my $sy_three_dots (@final_sy_three_dots) {
    my $aid = $sy_three_dots->findvalue ('following-sibling::Int[1]/@x');
    my $nr = $sy_three_dots->findvalue ('following-sibling::Int[2]/@x');
    unless (defined $aid && defined $nr) {
      print 'Error: we failed to extract either the first or the second Int following an syThreeDots element!', "\n";
      exit 1;
    }
    $definitions{"$aid:$nr"} = 0;
  }

  if (-e $article_eth) {
    if ($verbose == 1) {
      print 'Minimizing eth of ', $article_basename, ' quickly...';
    }
    my $eth_doc = undef;
    eval {
      $eth_doc = $xml_parser->parse_file ($article_eth);
    };
    if ($@) {
      print 'Error: the .eth file of ', $article_basename, ' is not well-formed XML.', "\n";
      exit 1;
    }

    # Create the new .eth document
    my $new_eth_doc = XML::LibXML::Document->createDocument ();
    my $eth_root = $new_eth_doc->createElement ('Theorems');

    $eth_root->setAttribute ('aid', $aid);
    $eth_root->appendText ("\n");
    my @theorem_nodes = $eth_doc->findnodes ('Theorems/Theorem');
    my $num_needed = 0;
    foreach my $theorem_node (@theorem_nodes) {
      unless ($theorem_node->exists ('@articlenr')) {
        print 'Error: we found a Theorem node that lacks an articlenr attribute!', "\n";
        exit 1;
      }
      unless ($theorem_node->exists ('@nr')) {
        print 'Error: we found a Theorem node that lacks an nr attribute!', "\n";
        exit 1;
      }
      unless ($theorem_node->exists ('@kind')) {
        print 'Error: we found a Theorem node that lacks a kind attribute!', "\n";
        exit 1;
      }
      my $articlenr = $theorem_node->findvalue ('@articlenr');
      my $nr = $theorem_node->findvalue ('@nr');
      my $kind = $theorem_node->findvalue ('@kind');
      if ($kind eq 'T') {
        if (defined $theorems{"${articlenr}:${nr}"}) {
          $num_needed++;
          $eth_root->appendChild ($theorem_node);
          $eth_root->appendText ("\n");
        }
      } elsif ($kind eq 'D') {
        if (defined $definitions{"${articlenr}:${nr}"}) {
          $num_needed++;
          $eth_root->appendChild ($theorem_node);
          $eth_root->appendText ("\n");
        }
      }
    }
    $new_eth_doc->setDocumentElement ($eth_root);
    $new_eth_doc->toFile ($article_eth);

    if ($verbose == 1) {
      print 'done.  The initial environment of ', $article_basename, ' contained ', scalar @theorem_nodes, ' elements, but we actually need only ', $num_needed, "\n";
    }

  } else {
    if ($verbose == 1) {
      print 'The .eth file does not exist for ', $article_basename, ', so there is nothing to minimize.', "\n";
    }
  }
}

# Ugh: This is nearly identical to the previous subroutine definition.
sub prune_schemes {
  my $refx_doc = undef;
  if (! defined eval { $refx_doc = $xml_parser->parse_file ($article_refx) } ) {
    print 'Error: the .refx file of ', $article_basename, ' is not well-formed XML.', "\n";
    exit 1;
  }
  my %schemes = ();
  my @initial_sy_three_dots = $refx_doc->findnodes ('Parser/syThreeDots[not(preceding-sibling::sySemiColon)]');
  foreach my $sy_three_dots (@initial_sy_three_dots) {
    my $aid = $sy_three_dots->findvalue ('following-sibling::Int[1]/@x');
    my $nr = $sy_three_dots->findvalue ('following-sibling::Int[2]/@x');
    unless (defined $aid && defined $nr) {
      print 'Error: we failed to extract either the first or the second Int following an syThreeDots element!', "\n";
      exit 1;
    }
    $schemes{"$aid:$nr"} = 0;
  }
  if (-e $article_esh) {
    if ($verbose == 1) {
      print 'Minimizing esh of ', $article_basename, ' quickly...';
    }
    my $esh_doc = undef;
    eval {
      $esh_doc = $xml_parser->parse_file ($article_esh);
    };
    if ($@) {
      print 'Error: the .esh file of ', $article_basename, ' is not well-formed XML.', "\n";
      exit 1;
    }

    # Create the new .esh document
    my $new_esh_doc = XML::LibXML::Document->createDocument ();
    my $esh_root = $new_esh_doc->createElement ('Schemes');

    $esh_root->setAttribute ('aid', $aid);
    $esh_root->appendText ("\n");
    my @scheme_nodes = $esh_doc->findnodes ('Schemes/Scheme');
    my $num_needed = 0;
    foreach my $scheme_node (@scheme_nodes) {
      unless ($scheme_node->exists ('@articlenr')) {
        print "\n" if ($verbose == 1);
        print 'Error: we found a Scheme node that lacks an articlenr attribute!', "\n";
        exit 1;
      }
      unless ($scheme_node->exists ('@nr')) {
        print "\n" if ($verbose == 1);
        print 'Error: we found a Scheme node that lacks an nr attribute!', "\n";
        exit 1;
      }
      my $articlenr = $scheme_node->findvalue ('@articlenr');
      my $nr = $scheme_node->findvalue ('@nr');
      if (defined $schemes{"${articlenr}:${nr}"}) {
        $num_needed++;
        $esh_root->appendChild ($scheme_node);
        $esh_root->appendText ("\n");
      }
    }
    $new_esh_doc->setDocumentElement ($esh_root);
    $new_esh_doc->toFile ($article_esh);

    if ($verbose == 1) {
      print 'done.  The initial environment of ', $article_basename, ' contained ', scalar @scheme_nodes, ' elements, but we actually need only ', $num_needed, "\n";
    }

  } else {
    if ($verbose == 1) {
      print 'The .esh file does not exist for ', $article_basename, ', so there is nothing to minimize.', "\n";
    }
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
  my $root_element_name = shift;
  my $begin = shift;
  my $end = shift;
  # DEBUG
  # print 'begin = ', $begin, ' and end = ', $end, "\n";
  if ($end < $begin) {
    return \%table;
  } elsif ($end == $begin) {
    # Try deleting
    delete $table{$begin};
    write_element_table (\@elements, \%table, $path, $root_element_name);
    my $deletable = our_verify ();
    if ($deletable) {
      if ($verbose) {
	print 'We can dump ', $root_element_name, ' element #', $begin, "\n";
      }
    } else {
      if ($verbose) {
	print 'We cannot dump ', $root_element_name, ' element #', $begin, "\n";
      }
      $table{$begin} = 0;
      write_element_table (\@elements, \%table, $path, $root_element_name);
    }
    return \%table;
  } elsif ($begin + 1 == $end) {

    delete $table{$begin};
    write_element_table (\@elements, \%table, $path, $root_element_name);
    my $begin_deletable = our_verify ();

    if ($begin_deletable) {
      if ($verbose) {
	print 'We can dump ', $root_element_name, ' element #', $begin, "\n";
      }
    } else {
      if ($verbose) {
	print 'We cannot dump ', $root_element_name, ' element #', $begin, "\n";
      }
      $table{$begin} = 0;
      write_element_table (\@elements, \%table, $path, $root_element_name);
    }

    delete $table{$end};
    write_element_table (\@elements, \%table, $path, $root_element_name);
    my $end_deletable = our_verify ();
    if ($end_deletable) {
      if ($verbose) {
	print 'We can dump ', $root_element_name, ' element #', $end, "\n";
      }
    } else {
      if ($verbose) {
	print 'We cannot dump ', $root_element_name, ' element #', $end, "\n";
      }
      $table{$end} = 0;
      write_element_table (\@elements, \%table, $path, $root_element_name);
    }

    return \%table;

  } else {

    my $segment_length = $end - $begin + 1;
    my $half_segment_length = floor ($segment_length / 2);

    # Dump the lower half
    foreach my $i ($begin .. $begin + $half_segment_length) {
      delete $table{$i};
    }

    # Write this to disk
    write_element_table (\@elements, \%table, $path, $root_element_name);

    # Check that deleting the lower half is safe
    my $lower_half_safe = our_verify ();
    if ($lower_half_safe) {
      # sleep 3;
      return (minimize (\@elements, \%table, $path, $root_element_name, $begin + $half_segment_length + 1, $end));
    } else {
      # Restore the lower half
      foreach my $i ($begin .. $begin + $half_segment_length) {
        $table{$i} = 0;
      }
      write_element_table (\@elements, \%table, $path, $root_element_name);
      # Minimize just the lower half
      # sleep 3;
      my %table_for_lower_half
        = %{minimize (\@elements, \%table, $path, $root_element_name, $begin, $begin + $half_segment_length)};
      # sleep 3;
      return (minimize (\@elements, \%table_for_lower_half, $path, $root_element_name, $begin + $half_segment_length + 1, $end));
    }
  }
}

sub minimize_by_article {
  my @elements = @{shift ()};
  my @articles = @{shift ()};
  my %table = %{shift ()};
  my $path = shift;
  my $root_element_name = shift;
  my $begin = shift;
  my $end = shift;
  # DEBUG
  # print 'begin = ', $begin, ' and end = ', $end, "\n";
  if ($end < $begin) {
    return \%table;
  } elsif ($end == $begin) {
    # Try deleting all items from the article
    my $article = $articles[$begin];
    foreach my $i (0 .. scalar @elements - 1) {
      my $element = $elements[$i];
      my $aid = aid_for_element ($element);
      if ($aid eq $article) {
	delete $table{$i};
      }
    }
    # Save the table to disk
    write_element_table (\@elements, \%table, $path, $root_element_name);
    my $deletable = our_verify ();
    if ($deletable) {
      if ($verbose) {
	print 'We can dump all ', $root_element_name, ' elements from ', $article, "\n";
      }
    } else {
      if ($verbose == 1) {
	print 'We cannot dump all ', $root_element_name, ' elements from ', $article, "\n";
      }
      # Restore all elements from $article
      foreach my $i (0 .. scalar @elements - 1) {
	my $element = $elements[$i];
	my $aid = aid_for_element ($element);
	if ($aid eq $article) {
	  $table{$i} = 0;
	}
      }
      write_element_table (\@elements, \%table, $path, $root_element_name);
    }
    return \%table;
  } elsif ($begin + 1 == $end) {

    my $begin_article = $articles[$begin];
    foreach my $i (0 .. scalar @elements - 1) {
      my $element = $elements[$i];
      my $aid = aid_for_element ($element);
      if ($aid eq $begin_article) {
	delete $table{$i};
      }
    }

    write_element_table (\@elements, \%table, $path, $root_element_name);
    my $begin_deletable = our_verify ();

    if ($begin_deletable) {
      if ($verbose == 1) {
	print 'We can dump all ', $root_element_name, ' elements from ', $begin_article, "\n";
      }
    } else {
      if ($verbose == 1) {
	print 'We cannot dump all ', $root_element_name, ' elements from article ', $begin_article, "\n";
      }

      foreach my $i (0 .. scalar @elements - 1) {
	my $element = $elements[$i];
	my $aid = aid_for_element ($element);
	if ($aid eq $begin_article) {
	  $table{$i} = 0;
	}
      }

      write_element_table (\@elements, \%table, $path, $root_element_name);
    }

    my $end_article = $articles[$end];

    foreach my $i (0 .. scalar @elements - 1) {
      my $element = $elements[$i];
      my $aid = aid_for_element ($element);
      if ($aid eq $end_article) {
	delete $table{$i};
      }
    }

    write_element_table (\@elements, \%table, $path, $root_element_name);

    my $end_deletable = our_verify ();
    if ($end_deletable) {
      if ($verbose == 1) {
	print 'We can dump all ', $root_element_name, ' elements from the article ', $end_article, "\n";
      }
    } else {
      if ($verbose == 1) {
	print 'We cannot dump all ', $root_element_name, ' elements from the article ', $end_article, "\n";
      }

      foreach my $i (0 .. scalar @elements - 1) {
	my $element = $elements[$i];
	my $aid = aid_for_element ($element);
	if ($aid eq $end_article) {
	  $table{$i} = 0;
	}
      }

      write_element_table (\@elements, \%table, $path, $root_element_name);
    }

    return \%table;

  } else {

    my $segment_length = $end - $begin + 1;
    my $half_segment_length = floor ($segment_length / 2);

    # Dump the lower half
    foreach my $i ($begin .. $begin + $half_segment_length) {
      my $article = $articles[$i];
      foreach my $i (0 .. scalar @elements - 1) {
	my $element = $elements[$i];
	my $aid = aid_for_element ($element);
	if ($aid eq $article) {
	  delete $table{$i};
	}
      }
    }

    # Write this to disk
    write_element_table (\@elements, \%table, $path, $root_element_name);

    # Check that deleting the lower half is safe
    my $lower_half_safe = our_verify ();
    if ($lower_half_safe) {
      if ($verbose == 1) {
	foreach my $i ($begin .. $begin + $half_segment_length) {
	  my $article = $articles[$i];
	  print 'We can dump all ', $root_element_name, ' elements from ', $article, "\n";
	}
      }
      return (minimize_by_article (\@elements, \@articles, \%table, $path, $root_element_name, $begin + $half_segment_length + 1, $end));
    } else {

      # Restore the lower half
      foreach my $i ($begin .. $begin + $half_segment_length) {
	my $article = $articles[$i];
	foreach my $i (0 .. scalar @elements - 1) {
	  my $element = $elements[$i];
	  my $aid = aid_for_element ($element);
	  if ($aid eq $article) {
	    $table{$i} = 0;
	  }
	}
      }

      write_element_table (\@elements, \%table, $path, $root_element_name);

      # Minimize just the lower half
      # sleep 3;
      my %table_for_lower_half
        = %{minimize_by_article (\@elements, \@articles, \%table, $path, $root_element_name, $begin, $begin + $half_segment_length)};
      # sleep 3;
      return (minimize_by_article (\@elements, \@articles, \%table_for_lower_half, $path, $root_element_name, $begin + $half_segment_length + 1, $end));
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

sub minimize_extension {
  my $extension_to_minimize = shift;
  my $root_element_name = $extension_to_element_table{$extension_to_minimize};
  if (defined $root_element_name) {
    my $article_with_extension = "${article_dirname}/${article_basename}.${extension_to_minimize}";
    if (-e $article_with_extension) {
      my $xml_doc;
      eval {
	$xml_doc = $xml_parser->parse_file ($article_with_extension);
      };
      if ($@) {
	print 'Error: the .', $extension_to_minimize, ' file of ', $article_basename, ' is not well-formed XML.', "\n";
	exit 1;
      }
      my @elements = $xml_doc->findnodes ("/${root_element_name}/*");
      my %initial_table = ();
      foreach my $i (0 .. scalar @elements - 1) {
        $initial_table{$i} = 0;
      }

      if ($verbose == 1) {
        print 'Minimizing ', $extension_to_minimize, ' of ', $article_basename, '...';
      }

      # Try to remove whole articles, i.e., remove all imported items
      # from a given article

      # First, harvest the articles that generated items in the current environment
      my %seen_articles = ();
      foreach my $element (@elements) {
	my $aid = aid_for_element ($element);
	if ($aid eq '') {
	  print 'Error: we found an element that lacks an aid attribute!', "\n";
	  exit 1;
	} else {
	  unless (defined $seen_articles{$aid}) {
	    $seen_articles{$aid} = 0;
	  }
	}
      }

      my @articles = keys %seen_articles;

      if ($verbose == 1) {
	print 'The current environment file has elements coming from ', scalar @articles, ' article(s).', "\n";
      }

      my $num_initial_elements = scalar keys %initial_table;

      my %minimized_by_article_table = %{minimize_by_article (\@elements, \@articles, \%initial_table, $article_with_extension, $root_element_name, 0, scalar @articles - 1)};

      my $num_elements_post_whole_article_deletion = scalar keys %minimized_by_article_table;

      if ($verbose == 1) {
	print 'Done eliminating whole articles from the .', $extension_to_minimize, ' file for ', $article_basename, '.  We started with ', $num_initial_elements, ' elements, but thanks to entire-article deletion, we have reduced this to ', $num_elements_post_whole_article_deletion, '.', "\n";
      }

      my %minimized_table
        = %{minimize (\@elements, \%minimized_by_article_table, $article_with_extension, $root_element_name, 0, scalar @elements - 1)};
      if ($verbose == 1) {
        print 'done.  The initial environment contained ', scalar @elements, ' elements, but we actually need only ', scalar keys %minimized_table, "\n";
      }
      my @removable = keys %minimized_table;
      return \@removable;
    } else {
      if ($verbose == 1) {
        print 'The .', $extension_to_minimize, ' file for ', $article_basename, ' does not exist; nothing to minimize.', "\n";
      }
      my @removable = ();
      return \@removable;
    }
  } else {
    print 'Error: we do not know how to deal with the ', $extension_to_minimize, ' files.', "\n";
    exit 1;
  }
}

sub confirm_minimality_of_extension {
  my $extension_to_minimize = shift;
  my $root_element_name = $extension_to_element_table{$extension_to_minimize};
  my @removable = ();
  if (defined $root_element_name) {
    my $article_with_extension = "${article_dirname}/${article_basename}.${extension_to_minimize}";
    if (-e $article_with_extension) {
      my $xml_doc = undef;
      eval {
	$xml_doc = $xml_parser->parse_file ($article_with_extension);
      };
      if ($@) {
	print 'Error: the .', $extension_to_minimize, ' file of ', $article_basename, ' is not well-formed XML.', "\n";
	exit 1;
      }
      my @elements = $xml_doc->findnodes ("/${root_element_name}/*");

      my %needed_elements_table = ();
      foreach my $i (0 .. scalar @elements - 1) {
        $needed_elements_table{$i} = 0;
      }

      my $removable_element_found = 0;
      my $i = 0;
      my $num_elements = scalar @elements;

      # DEBUG
      # print 'We are about to inspect ', scalar @elements, ' elements, checking for removability.', "\n";

      while ($i < $num_elements && $removable_element_found == 0) {

	my $element = $elements[$i];

	# DEBUG
	my $element_pretty = render_element ($element);
	# print 'Checking whether the element ', $element_pretty, ' can be removed from the .', $extension_to_minimize, ' file of ', $article_basename, '...';

	delete $needed_elements_table{$i};
	write_element_table (\@elements, \%needed_elements_table, $article_with_extension, $root_element_name);

	my $verifier_ok = our_verify ();
	if ($verifier_ok) {

	  # DEBUG
	  # print 'removable!', "\n";

	  my $element_pretty = render_element ($element);
	  push (@removable, $element_pretty);
	  $removable_element_found = 1;
	} else {
	  # DEBUG
	  # print 'unremovable!', "\n";
	}

	$needed_elements_table{$i} = 0;
	write_element_table (\@elements, \%needed_elements_table, $article_with_extension, $root_element_name);

	$i++;

      }
    }

    # Return the XML, etc. to its initial state
    our_verify ();

    return \@removable;

  } else {
    print 'Error: we do not know how to deal with the ', $extension_to_minimize, ' files.', "\n";
    exit 1;
  }
}

sub minimize_properties {
    my $minimize_properties_out = '';
    my $minimize_properties_err = '';
    my @minimize_properties_call = ($minimize_properties_script);
    if ($checker_only) {
	push (@minimize_properties_call, '--checker-only');
    }
    push (@minimize_properties_call, $article_miz);

    run (\@minimize_properties_call,
	 '>', '/dev/null',
	 '2>', \$minimize_properties_err)
	or croak ('Error: the property minimization script did not exit cleanly for ', $article_basename, '.  Here are the errors it emitted:', "\n", $minimize_properties_err);
}

sub minimize_abstractness {
    my $minimize_abstractness_out = '';
    my $minimize_abstractness_err  ='';
    my @minimize_abstractness_call = ($minimize_abstractness_script);
    if ($checker_only) {
	push (@minimize_abstractness_call, '--checker-only');
    }
    push (@minimize_abstractness_call, $article_miz);

    run (\@minimize_abstractness_call,
	 '>', '/dev/null',
	 '2>', \$minimize_abstractness_err)
	or croak ('Error: the abstractness minimization script did not exit cleanly for ', $article_basename, '.  Here are the errors it emitted:', "\n", $minimize_abstractness_err);
}

# Let's do it

if (! $confirm_only) {

  if ($paranoid) {

      if (! our_verify ()) {
	  croak('Error: ', $article_basename, ' is not verifiable.');
      }

      if ($verbose) {
	  print 'Paranoia: We have confirmed that, before minimization, ', $article_basename, ' is verifiable.', "\n";
      }
  }

  if ($fast_schemes) {
    prune_schemes ();
  } else {
    push (@extensions_to_minimize, 'esh');
  }

  if ($fast_theorems) {
    prune_theorems ();
  } else {
    push (@extensions_to_minimize, 'eth');
  }

  foreach my $extension_to_minimize (@extensions_to_minimize) {
    minimize_extension ($extension_to_minimize);
  }

  # Restore
  our_verify ()
      or croak ('Error: we are unable to verify ', $article_basename, ' in its newly minimized environment, before minimizing constructor properties.');

  minimize_properties ();

  # We are pretty close to the end.  But there might be spurious
  # dependencies on abstractness properties.  Dump as many as we can.

  # Restore
  our_verify ()
      or croak ('Error: we are unable to verify ', $article_basename, ' in its newly minimized environment, before minimizing abstractness properties.');

  minimize_abstractness ();

}

if ($paranoid or $confirm_only) {

  # Check that the XML is valid
  if (! defined eval { $xml_parser->parse_file ($article_xml) } ) {
    croak ('Error: before confirming the minimality of the environment, we are left with an invalid XML file at ', $article_xml, '.', "\n");
  }

  if ($verbose == 1) {
    print 'Confirming minimality...', "\n";
  }

  my @extensions_to_minimize = ('eno', 'erd', 'epr', 'dfs', 'eid', 'ecl', 'esh', 'eth');
  my %removable_by_extension = ();
  foreach my $extension (@extensions_to_minimize) {

    if ($verbose == 1) {
      print 'Confirmining minimality of the .', $extension, ' file...';
    }

    my @removable = @{confirm_minimality_of_extension ($extension)};

    $removable_by_extension{$extension} = \@removable;

    if ($verbose == 1) {
      print 'done.', "\n";
    }
  }

  my $some_extension_unminimized = 0;

  foreach my $extension (keys %removable_by_extension) {
    my @removable = @{$removable_by_extension{$extension}};
    unless (scalar @removable == 0) {
      my $removable_item = $removable[0];
      print 'Error: the .', $extension, ' file of the article ', $article_basename, ' is not minimized: ', $removable_item, ' can be safely deleted (and possiby others, too).', "\n";
      $some_extension_unminimized = 1;
    }
  }

  if ($some_extension_unminimized == 1) {
    exit 1;
  }

}

our_verify ()
    or croak ('Error: we are unable to verify ', $article_basename, ' in its newly minimized environment.');

__END__

=cut

=head1 minimal.pl

minimal.pl - Minimize the environment of a mizar article

=head1 SYNOPSIS

minimize.pl [options] mizar-article

=head1 OPTIONS

=over 8

=item B<--help>

Prints a brief help message and exits.

=item B<--man>

Prints the manual page and exits.

=item B<--verbose>

Say what environment file we're minimizing, and for each environment
file, say how many environment "items" are present there and how many
we really need.

=item B<--debug>

Be very verbose about what is going on, for debugging purposes.

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

=cut
