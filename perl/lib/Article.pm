package Article;

use Moose;
use Carp qw(croak carp);
use File::Basename qw(basename dirname);
use File::Copy qw(); # import nothing; we define our own 'copy' subroutine
use Regexp::DefaultFlags;
use XML::LibXML;
use POSIX qw(floor ceil);

# Our libraries
use FindBin;
use lib $FindBin::Bin;
use Utils qw(ensure_readable_file);
use Mizar;
use Xsltproc qw(apply_stylesheet);

has 'path' => (
    is => 'ro',
    isa => 'Str',
    reader => 'get_path',
    required => 1,
);

my $xml_parser = XML::LibXML->new (suppress_errors => 1,
				   suppress_warnings => 1);

sub BUILD {
    my $self = shift;

    my $path = $self->get_path ();
    if ( ! ensure_readable_file ($path)) {
	croak ('Error: the assigned path (', $path, ') is not a readable file.');
    }
}

sub name {
    my $self = shift;
    return basename ($self->get_path (), '.miz');
}

sub aid {
    my $self = shift;
    my $name = $self->name ();
    return uc $name;
}

sub get_dirname {
    my $self = shift;
    return dirname ($self->get_path ());
}

sub file_with_extension {
    my $self = shift;
    my $extension = shift;

    if (! defined $extension) {
	croak ('Error: please supply an extension.');
    }

    my $dir = $self->get_dirname ();
    my $name = $self->name ();

    my $name_with_extension = "${name}.${extension}";
    my $path_with_extension = "${dir}/${name_with_extension}";

    return $path_with_extension;

}

sub absolutize {
    my $self = shift;
    $self->absolutize_extension ('xml');
}

sub absolutize_extension {
    my $self = shift;
    my $extension = shift;
    my $xml_path = $self->file_with_extension ($extension);
    my $abs_xml_path = $self->file_with_extension ("${extension}1");

    if (-e $xml_path) {
	my $absrefs_stylesheet = Mizar::path_for_stylesheet ('addabsrefs');

	return (apply_stylesheet ($absrefs_stylesheet,
				  $xml_path,
				  $abs_xml_path));
    } else {
	croak ('Error: there is no .', $extension, ' file for ', $self->name (), '.');
    }
}

sub absolutize_environment {
    my $self = shift;

    foreach my $extension ('eno', 'dfs', 'ecl', 'eid', 'epr', 'erd', 'esh', 'eth') {
	my $file = $self->file_with_extension ($extension);
	if (-e $file) {
	    $self->absolutize_extension ($extension);
	}
    }

    return 1;
}

# sub minimize_abstractness {
#     my $minimize_abstractness_out = '';
#     my $minimize_abstractness_err  ='';
#     my @minimize_abstractness_call = ($minimize_abstractness_script);
#     if ($checker_only) {
# 	push (@minimize_abstractness_call, '--checker-only');
#     }
#     push (@minimize_abstractness_call, $article_miz);

#     run (\@minimize_abstractness_call,
# 	 '>', '/dev/null',
# 	 '2>', \$minimize_abstractness_err)
# 	or croak ('Error: the abstractness minimization script did not exit cleanly for ', $article_basename, '.  Here are the errors it emitted:', "\n", $minimize_abstractness_err);
# }

# sub confirm_minimality_of_extension {
#   my $extension_to_minimize = shift;
#   my $root_element_name = $extension_to_element_table{$extension_to_minimize};
#   my @removable = ();
#   if (defined $root_element_name) {
#     my $article_with_extension = "${article_dirname}/${article_basename}.${extension_to_minimize}";
#     if (-e $article_with_extension) {
#       my $xml_doc = undef;
#       eval {
# 	$xml_doc = $xml_parser->parse_file ($article_with_extension);
#       };
#       if ($@) {
# 	print 'Error: the .', $extension_to_minimize, ' file of ', $article_basename, ' is not well-formed XML.', "\n";
# 	exit 1;
#       }
#       my @elements = $xml_doc->findnodes ("/${root_element_name}/*");

#       my %needed_elements_table = ();
#       foreach my $i (0 .. scalar @elements - 1) {
#         $needed_elements_table{$i} = 0;
#       }

#       my $removable_element_found = 0;
#       my $i = 0;
#       my $num_elements = scalar @elements;

#       # DEBUG
#       # print 'We are about to inspect ', scalar @elements, ' elements, checking for removability.', "\n";

#       while ($i < $num_elements && $removable_element_found == 0) {

# 	my $element = $elements[$i];

# 	# DEBUG
# 	my $element_pretty = render_element ($element);
# 	# print 'Checking whether the element ', $element_pretty, ' can be removed from the .', $extension_to_minimize, ' file of ', $article_basename, '...';

# 	delete $needed_elements_table{$i};
# 	write_element_table (\@elements, \%needed_elements_table, $article_with_extension, $root_element_name);

# 	my $verifier_ok = our_verify ();
# 	if ($verifier_ok) {

# 	  # DEBUG
# 	  # print 'removable!', "\n";

# 	  my $element_pretty = render_element ($element);
# 	  push (@removable, $element_pretty);
# 	  $removable_element_found = 1;
# 	} else {
# 	  # DEBUG
# 	  # print 'unremovable!', "\n";
# 	}

# 	$needed_elements_table{$i} = 0;
# 	write_element_table (\@elements, \%needed_elements_table, $article_with_extension, $root_element_name);

# 	$i++;

#       }
#     }

#     # Return the XML, etc. to its initial state
#     our_verify ();

#     return \@removable;

#   } else {
#     print 'Error: we do not know how to deal with the ', $extension_to_minimize, ' files.', "\n";
#     exit 1;
#   }
# }

sub needed_non_constructors {
    my $self = shift;

    $self->absolutize ();
    $self->absolutize_environment ();

    my $abs_xml = $self->file_with_extension ('xml1');

    my $dependencies_stylesheet = Mizar::path_for_stylesheet ('dependencies');

    my @needed = apply_stylesheet ($dependencies_stylesheet,
				   $abs_xml);

    # There may be repetitions here
    my %items = ();
    foreach my $item (@needed) {
	$items{$item} = 0;
    }

    if (wantarray) {
	return keys %items;
    } else {
	return join (' ', keys %items);
    }
}

sub needed_constructors {
    my $self = shift;
    my $abs_xml_path = $self->file_with_extension ('xml1');

    $self->absolutize ();
    $self->absolutize_environment ();

    my $inferred_constructors_stylesheet
	= Mizar::path_for_stylesheet ('inferred-constructors');

    my @all_constructors = apply_stylesheet ($inferred_constructors_stylesheet,
					     $abs_xml_path);

    my %constructors = ();
    foreach my $constructor (@all_constructors) {
	if (! defined $constructors{$constructor}) {
	    $constructors{$constructor} = 0;
	}
    }

    if (wantarray) {
	return keys %constructors;
    } else {
	return join (' ', keys %constructors);
    }

}

sub properties_of_constructor {
    my $self = shift;

    my $constructor = shift;

    if (! defined $constructor) {
	croak ('Error: please supply a constructor.');
    }

    if ($constructor =~ /\A ([0-9a-z_]+) : (.) constructor : ([0-9]+) \z/) {
	my ($aid, my $kind, my $nr) = ($1, $2, $3);

	my $atr = $self->file_with_extension ('atr');

	my %params =
	    (
		'kind' => $kind,
		'aid' => $aid,
		'nr' => $nr,
	    );

	my @props
	    = apply_stylesheet (Mizar::path_for_stylesheet ('properties-of-constructor'),
				$atr,
				undef,
				\%params);

	# carp ('Inside properties_of_constructor: ', join (' ', @props));

	if (wantarray) {
	    return @props;
	} else {
	    return join (' ', @props);
	}

    } else {
	croak ('Error: Unable to make sense of the constructor \'', $constructor, '\'.');
    }



}

sub properties_of_needed_constructors {
    my $self = shift;

    my @needed_constructors = $self->needed_constructors ();

    my @needed = ();

    foreach my $constructor (@needed_constructors) {
	my @props = $self->properties_of_constructor ($constructor);
	push (@needed, map { "${constructor}[$_]"; } @props);
    }

    if (wantarray) {
	return @needed;
    } else {
	return join (' ', @needed);
    }

}

sub show_errors {
    my $self = shift;

    my $err_file = $self->file_with_extension ('err');
    if (ensure_readable_file ($err_file)) {
	my $errs = `cat $err_file`;
	if (wantarray) {
	    return split ("\n", $errs);
	} else {
	    return $errs;
	}
    } else {
	carp ('Warning: there is no .err file for ', $self->name (), ' at the expected location (', $err_file, ').');
    }
}

sub accom {
    my $self = shift;
    return Mizar::accom ($self->get_path ());
}

sub is_accomable {
    my $self = shift;
    return $self->accom ();
}

sub verify {
    my $self = shift;
    return Mizar::verifier ($self->get_path ());
}

sub is_verifiable {
    my $self = shift;
    return $self->verify ();
}

sub wsmparser {
    my $self = shift;
    return Mizar::wsmparser ($self->get_path ());
}

sub msmprocessor {
    my $self = shift;
    return Mizar::msmprocessor ($self->get_path ());
}

sub msplit {
    my $self = shift;
    return Mizar::msplit ($self->get_path ());
}

sub mglue {
    my $self = shift;
    return Mizar::mglue ($self->get_path ());
}

sub minimize_properties {
    my $self = shift;

    my $parameters_hash_ref = shift;
    my %parameters = defined $parameters_hash_ref ? %{$parameters_hash_ref} : ();

    # Delete all properties of all constructors that are not needed
    my @needed_constructors = $self->needed_constructors ();

    my %needed_properties = ();
    my %unneeded_properties = ();

    my $miz = $self->file_with_extension ('miz');
    my $atr = $self->file_with_extension ('atr');
    my $atr_orig = $self->file_with_extension ('atr.orig');
    my $atr_tmp = $self->file_with_extension ('atr.tmp');

    # Save a copy of the atr before proceeding
    File::Copy::copy ($atr, $atr_orig)
	or croak ('Error: Unable to save a copy of ', $atr, ' to ', $atr_orig, '.');

    my $strip_property_stylesheet = Mizar::path_for_stylesheet ('strip-property');

    foreach my $constructor (@needed_constructors) {
	my @properties = $self->properties_of_constructor ($constructor);
	if ($constructor =~ /\A ([a-z0-9_]+) : (.) constructor : ([0-9]+) \z/x) {
	    (my $aid, my $kind, my $nr) = ($1, $2, $3);

	    foreach my $property (@properties) {
		my %params = (
		    'kind' => $kind,
		    'nr' => $nr,
		    'aid' => $aid,
		    'property' => $property,
		);
		apply_stylesheet ($strip_property_stylesheet,
				  $atr,
				  $atr_tmp,
				  \%params)
		    or croak ('Error: unable to apply the strip-property stylesheet to ', $atr);
		File::Copy::move ($atr_tmp, $atr);


		if ($self->verify ($miz, \%parameters)) {
		    $unneeded_properties{"${constructor}[${property}]"} = 0;
		    File::Copy::copy ($atr, $atr_orig)
			or croak ('Error: we were unable to update the .atr for ', $self->name (), ' to reflect its independence from the property ', $property, ' of constructor ', $constructor, '.', "\n");
		} else {
		    $needed_properties{"${constructor}[${property}]"} = 0;
		    File::Copy::copy ($atr_orig, $atr)
			or croak ('Error: we are unable to restore the original article .atr from ', $atr_orig, '.', "\n");
		}
	    }
	} else {
	    croak ('Error: unable to make sense of the constructor \'', $constructor, '\'.', "\n");
	}
    }

    if (wantarray) {
	return keys %needed_properties;
    } else {
	return join (' ', keys %needed_properties);
    }
}

sub write_element_table {

    my $self = shift;
    my @elements = @{shift ()};
    my %table = %{shift ()};
    my $path = shift;
    my $root_element_name = shift;

    my $new_doc = XML::LibXML::Document->createDocument ();
    my $root = $new_doc->createElement ($root_element_name);

    $root->setAttribute ('aid', $self->aid ());
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

sub prune_theorems_and_schemes {
    my $self = shift;

    my $refx_path = $self->file_with_extension ('refx');
    my $refx_doc = eval { $xml_parser->parse_file ($refx_path) };

    if (! defined $refx_doc) {
	croak ('Error: the .refx file of ', $self->name (), ' is not well-formed XML.');
    }

    my %schemes = ();
    my %theorems = ();
    my %definitions = ();

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

    my @middle_sy_three_dots = $refx_doc->findnodes ('Parser/syThreeDots[preceding-sibling::sySemiColon and following-sibling::sySemiColon[2]]');

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

    my $eth_file = $self->file_with_extension ('eth');

    if (-e $eth_file) {
	my $eth_doc = eval { $xml_parser->parse_file ($eth_file) };
	if (! defined $eth_doc) {
	    croak ('Error: the .eth file of ', $self->name (), ' is not well-formed XML.');
	}

	# Create the new .eth document
	my $new_eth_doc = XML::LibXML::Document->createDocument ();
	my $eth_root = $new_eth_doc->createElement ('Theorems');

	$eth_root->setAttribute ('aid', $self->aid ());
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
	$new_eth_doc->toFile ($eth_file);

    }

        my $esh_file = $self->file_with_extension ('esh');
    if (-e $esh_file) {
	my $esh_doc = eval { $xml_parser->parse_file ($esh_file) };
	if (! defined $esh_doc) {
	    croak 'Error: the .esh file of ', $self->name (), ' is not well-formed XML.', "\n";
	}

	# Create the new .esh document
	my $new_esh_doc = XML::LibXML::Document->createDocument ();
	my $esh_root = $new_esh_doc->createElement ('Schemes');

	$esh_root->setAttribute ('aid', $self->aid ());
	$esh_root->appendText ("\n");
	my @scheme_nodes = $esh_doc->findnodes ('Schemes/Scheme[@articlenr and @nr]');
	my $num_needed = 0;
	foreach my $scheme_node (@scheme_nodes) {
	    my $articlenr = $scheme_node->findvalue ('@articlenr');
	    my $nr = $scheme_node->findvalue ('@nr');
	    if (defined $schemes{"${articlenr}:${nr}"}) {
		$num_needed++;
		$esh_root->appendChild ($scheme_node);
		$esh_root->appendText ("\n");
	    }
	}
	$new_esh_doc->setDocumentElement ($esh_root);
	$new_esh_doc->toFile ($esh_file);

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

sub minimize_elements {
    my $self = shift;
    my @elements = @{shift ()};
    my %table = %{shift ()};
    my $path = shift;
    my $root_element_name = shift;
    my $begin = shift;
    my $end = shift;
    my $parameters_ref = shift;
    my %parameters = defined $parameters_ref ? %{$parameters_ref} : ();

    if ($end < $begin) {

	return \%table;

    } elsif ($end == $begin) {

	# Try deleting
	delete $table{$begin};

	$self->write_element_table (\@elements,
				    \%table,
				    $path,
				    $root_element_name);

	my $deletable = $self->verify (\%parameters);

	if (! $deletable) {
	    $table{$begin} = 0;
	    $self->write_element_table (\@elements,
					\%table,
					$path,
					$root_element_name);
	}

	return \%table;

    } elsif ($begin + 1 == $end) {

	delete $table{$begin};

	$self->write_element_table (\@elements,
				    \%table,
				    $path,
				    $root_element_name);

	my $begin_deletable = $self->verify (\%parameters);

	if (! $begin_deletable) {
	    $table{$begin} = 0;
	    $self->write_element_table (\@elements,
					\%table,
					$path,
					$root_element_name);
	}

	delete $table{$end};

	$self->write_element_table (\@elements,
				    \%table,
				    $path,
				    $root_element_name);

	my $end_deletable = $self->verify (\%parameters);

	if (! $end_deletable) {
	    $table{$end} = 0;
	    $self->write_element_table (\@elements,
					\%table,
					$path,
					$root_element_name);
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
	$self->write_element_table (\@elements,
				    \%table,
				    $path,
				    $root_element_name);

	# Check that deleting the lower half is safe
	my $lower_half_safe = $self->verify (\%parameters);

	if ($lower_half_safe) {

	    return ($self->minimize_elements (\@elements,
					      \%table,
					      $path,
					      $root_element_name,
					      $begin + $half_segment_length + 1,
					      $end,
					      \%parameters));

	} else {

	    # Restore the lower half
	    foreach my $i ($begin .. $begin + $half_segment_length) {
		$table{$i} = 0;
	    }

	    $self->write_element_table (\@elements,
					\%table,
					$path,
					$root_element_name);

	    # Minimize just the lower half
	    my %table_for_lower_half
		= %{$self->minimize_elements (\@elements,
					      \%table,
					      $path, $root_element_name,
					      $begin,
					      $begin + $half_segment_length,
					      \%parameters)};
	    return ($self->minimize_elements (\@elements,
					      \%table_for_lower_half,
					      $path,
					      $root_element_name,
					      $begin + $half_segment_length + 1,
					      $end,
					      \%parameters));
	}
    }
}

sub minimize_by_article {
    my $self = shift;
    my @elements = @{shift ()};
    my @articles = @{shift ()};
    my %table = %{shift ()};
    my $path = shift;
    my $root_element_name = shift;
    my $begin = shift;
    my $end = shift;
    my $parameters_ref = shift;
    my %parameters = defined $parameters_ref ? %{$parameters_ref} : ();

    if ($end < $begin) {

	return \%table;

    } elsif ($end == $begin) {

	# Try deleting all items from the article
	my $article = $articles[$begin];
	foreach my $i (0 .. scalar @elements - 1) {
	    my $element = $elements[$i];
	    my $aid = aid_for_element ($element);
	    if (defined $aid && $aid eq $article) {
		delete $table{$i};
	    }
	}

	# Save the table to disk
	$self->write_element_table (\@elements, \%table, $path, $root_element_name);

	my $deletable = $self->verify (\%parameters);

	if (! $deletable) {

	    # Restore all elements from $article
	    foreach my $i (0 .. scalar @elements - 1) {
		my $element = $elements[$i];
		my $aid = aid_for_element ($element);
		if (defined $aid && $aid eq $article) {
		    $table{$i} = 0;
		}
	    }

	    $self->write_element_table (\@elements,
					\%table,
					$path,
					$root_element_name);

	}
	return \%table;

    } elsif ($begin + 1 == $end) {

	my $begin_article = $articles[$begin];
	foreach my $i (0 .. scalar @elements - 1) {
	    my $element = $elements[$i];
	    my $aid = aid_for_element ($element);
	    if (defined $aid && $aid eq $begin_article) {
		delete $table{$i};
	    }
	}

	$self->write_element_table (\@elements, \%table, $path, $root_element_name);

	my $begin_deletable = $self->verify (\%parameters);

	if (! $begin_deletable) {

	    # Restore the beginning
	    foreach my $i (0 .. scalar @elements - 1) {
		my $element = $elements[$i];
		my $aid = aid_for_element ($element);
		if ($aid eq $begin_article) {
		    $table{$i} = 0;
		}
	    }

	    $self->write_element_table (\@elements,
					\%table,
					$path,
					$root_element_name);

	}

	my $end_article = $articles[$end];

	foreach my $i (0 .. scalar @elements - 1) {
	    my $element = $elements[$i];
	    my $aid = aid_for_element ($element);
	    if ($aid eq $end_article) {
		delete $table{$i};
	    }
	}

	$self->write_element_table (\@elements, \%table, $path, $root_element_name);

	my $end_deletable = $self->verify (\%parameters);

	if (! $end_deletable) {

	    # Restore the end
	    foreach my $i (0 .. scalar @elements - 1) {
		my $element = $elements[$i];
		my $aid = aid_for_element ($element);
		if ($aid eq $end_article) {
		    $table{$i} = 0;
		}
	    }

	    $self->write_element_table (\@elements,
					\%table,
					$path,
					$root_element_name);

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
		if (defined $aid && $aid eq $article) {
		    delete $table{$i};
		}
	    }
	}

	# Write this to disk
	$self->write_element_table (\@elements, \%table, $path, $root_element_name);

	# Check that deleting the lower half is safe
	my $lower_half_safe = $self->verify (\%parameters);

	if ($lower_half_safe) {

	    return ($self->minimize_by_article (\@elements,
						\@articles,
						\%table,
						$path,
						$root_element_name,
						$begin + $half_segment_length + 1,
						$end,
						\%parameters));

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

	    $self->write_element_table (\@elements,
					\%table,
					$path,
					$root_element_name);

	    # Minimize just the lower half
	    my %table_for_lower_half
		= %{$self->minimize_by_article (\@elements,
						\@articles,
						\%table,
						$path,
						$root_element_name,
						$begin,
						$begin + $half_segment_length,
						\%parameters)};
	    return ($self->minimize_by_article (\@elements,
						\@articles,
						\%table_for_lower_half,
						$path,
						$root_element_name,
						$begin + $half_segment_length + 1,
						$end,
						\%parameters));
	}
    }
}

sub aid_for_element {
  my $element = shift;
  $element->exists ('@aid') ? return $element->findvalue ('@aid') : return undef;
}

my %extension_to_element_table = ('eno' => 'Notations',
				  'erd' => 'ReductionRegistrations',
				  'epr' => 'PropertyRegistration',
				  'dfs' => 'Definientia',
				  'eid' => 'IdentifyRegistrations',
				  'ecl' => 'Registrations',
				  'esh' => 'Schemes',
				  'eth' => 'Theorems');

sub minimize_extension {
    my $self = shift;
    my $extension_to_minimize = shift;
    my $parameters_ref = shift;

    my %parameters = defined $parameters_ref ? %{$parameters_ref} : ();

    my $root_element_name = $extension_to_element_table{$extension_to_minimize};

    if (! defined $root_element_name) {
	croak ('Error: we do not know how to deal with the ', $extension_to_minimize, ' files.')
    }

    my $article_with_extension = $self->file_with_extension ($extension_to_minimize);

    if (-e $article_with_extension) {
	my $xml_doc = eval { $xml_parser->parse_file ($article_with_extension) };
	if (! defined $xml_doc) {
	    croak ('Error: the .', $extension_to_minimize, ' file of ', $self->name (), ' is not well-formed XML.');
	}

	my @elements = $xml_doc->findnodes ("/${root_element_name}/*");

	my %initial_table = ();

	# Initially, everything is needed.
	foreach my $i (0 .. scalar @elements - 1) {
	    $initial_table{$i} = 0;
	}

	# Try to remove whole articles, i.e., remove all imported items
	# from a given article

	# First, harvest the articles that generated items in the current environment
	my %seen_articles = ();
	foreach my $element (@elements) {
	    my $aid = aid_for_element ($element);

	    if (! defined $aid) {
		croak ('Error: we found an element that lacks an aid attribute!');
	    }

	    $seen_articles{$aid} = 0;
	}

	my @articles = keys %seen_articles;

	my $num_initial_elements = scalar keys %initial_table;

	my %minimized_by_article_table
	    = %{$self->minimize_by_article (\@elements,
					    \@articles,
					    \%initial_table,
					    $article_with_extension,
					    $root_element_name,
					    0,
					    scalar @articles - 1,
					    \%parameters)};

	my $num_elements_post_whole_article_deletion
	    = scalar keys %minimized_by_article_table;

	my %minimized_table
	    = %{$self->minimize_elements (\@elements,
					  \%minimized_by_article_table,
					  $article_with_extension, $root_element_name,
					  0,
					  scalar @elements - 1,
					  \%parameters)};

	return keys %minimized_table;

    } else {
	return ();
    }
}

sub minimize_environment {

    my $self = shift;
    my $parameters_ref = shift;

    my %parameters = defined $parameters_ref ? %{$parameters_ref} : ();

    my @extensions_to_minimize = ('eno', 'erd', 'epr', 'dfs', 'eid', 'ecl');

    if (defined $parameters{'fast-theorems-and-schemes'}) {
	prune_theorems_and_schemes ();
    } else {
	push (@extensions_to_minimize, 'esh', 'eth');
    }

    foreach my $extension (@extensions_to_minimize) {
	$self->minimize_extension ($extension, \%parameters);
    }

}

sub minimize {
    my $self = shift;
    my $parameters_ref = shift;

    my %parameters = defined $parameters_ref ? %{$parameters_ref} : ();

    $self->minimize_environment (\%parameters);
    $self->minimize_properties (\%parameters);

    return 1;
}

sub needed_items {

    my $self = shift;

    my @needed = ();

    my @non_constructor_dependencies = $self->needed_non_constructors ();
    my @constructor_dependencies = $self->needed_constructors ();
    my @property_dependencies = $self->properties_of_needed_constructors ();

    push (@needed, @non_constructor_dependencies);
    push (@needed, @constructor_dependencies);
    push (@needed, @property_dependencies);

    if (wantarray) {
	return @needed;
    } else {
	return join (' ', @needed);
    }

}

sub itemize {
    my $self = shift;

    return 1;
}

sub copy {
    my $self = shift;
    my $new_path = shift;

    my $current_path = $self->get_path ();

    File::Copy::copy ($current_path, $new_path)
	or croak ('Error: unable to copy ', $current_path, ' to ', $new_path, '.');

    my $new_path_dirname = dirname ($new_path);
    my $new_path_basename = basename ($new_path, '.miz');

    my $new_path_miz = "${new_path_dirname}/${new_path_basename}.miz";

    my $new_article = $self->new (path => $new_path_miz);

    return $new_article;

}

1;
