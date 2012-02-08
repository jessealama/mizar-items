package Article;

use Moose;
use Carp qw(croak carp);
use File::Basename qw(basename dirname);
use File::Copy qw(); # import nothing; we define our own 'copy' subroutine
use Regexp::DefaultFlags;
use XML::LibXML;
use POSIX qw(floor ceil);
use Data::Dumper;

# Our libraries
use FindBin;
use lib $FindBin::Bin;
use Utils qw(ensure_readable_file ensure_directory strip_extension extension);
use Mizar;
use ItemizedArticle;
use LocalDatabase;
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

    if (-e $xml_path && ! -e $abs_xml_path) {
	my $absrefs_stylesheet = Mizar::path_for_stylesheet ('addabsrefs');

	return (apply_stylesheet ($absrefs_stylesheet,
				  $xml_path,
				  $abs_xml_path));
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

    # warn 'Computing needed constructors; looking in ', $abs_xml_path;

    if (! ensure_readable_file ($abs_xml_path)) {
	croak ('Error: the absolute XML does not exist (or is unreadable).');
    }

    my $inferred_constructors_stylesheet
	= Mizar::path_for_stylesheet ('inferred-constructors');

    my @all_constructors = apply_stylesheet ($inferred_constructors_stylesheet,
					     $abs_xml_path);

    # warn 'Results of the inferred-constructors stylesheet:', Dumper (@all_constructors);

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
    my $parameters_ref = shift;

    my %parameters = defined $parameters_ref ? %{$parameters_ref} : ();
    return Mizar::verifier ($self->get_path (),
			    \%parameters);
}

sub is_verifiable {
    my $self = shift;
    my $parameters_ref = shift;

    my %parameters = defined $parameters_ref ? %{$parameters_ref} : ();
    return $self->verify (\%parameters);
}

sub wsmparser {
    my $self = shift;
    return Mizar::wsmparser ($self->get_path ());
}

sub export {
    my $self = shift;
    return Mizar::exporter ($self->get_path ());
}

sub is_exportable {
    my $self = shift;
    return $self->export ();
}

sub transfer {
    my $self = shift;
    return Mizar::transfer ($self->get_path ());
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

    # warn 'Needed constructors:', "\n", Dumper (@needed_constructors);

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

	# warn 'Properties of ', $constructor, ':', "\n", Dumper (@properties);

	if ($constructor =~ /\A ([a-z0-9_]+) : (.) constructor : ([0-9]+) \z/x) {
	    (my $aid, my $kind, my $nr) = ($1, $2, $3);

	    foreach my $property (@properties) {
		apply_stylesheet ($strip_property_stylesheet,
				  $atr,
				  $atr_tmp,
				  {
				      'kind' => $kind,
				      'nr' => $nr,
				      'aid' => $aid,
				      'property' => $property,
				  }
			      )
		    or croak ('Error: unable to apply the strip-property stylesheet to ', $atr);
		File::Copy::move ($atr_tmp, $atr);


		if ($self->verify (\%parameters)) {
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

	# Restore
	$self->verify (\%parameters);

	my $num_elements_post_whole_article_deletion
	    = scalar keys %minimized_by_article_table;

	my %minimized_table
	    = %{$self->minimize_elements (\@elements,
					  \%minimized_by_article_table,
					  $article_with_extension, $root_element_name,
					  0,
					  scalar @elements - 1,
					  \%parameters)};

	# Restore
	$self->verify (\%parameters);

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

    $self->absolutize_environment ();

    my @needed = ();

    my @non_constructor_dependencies = $self->needed_non_constructors ();

    my $name = $self->name ();
    # warn 'Needed non-constructors:', "\n", Dumper (@non_constructor_dependencies);

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

sub list_as_token_string {
  my @lst = @{shift ()};
  my $val = '';
  my $num_elements = scalar @lst;
  for (my $i = 0; $i < $num_elements; $i++) {
    $val .= $lst[$i];
    $val .= ',';
  }
  return $val;
}

sub fragment_number {
  my $fragment_path = shift;
  if ($fragment_path =~ m{ \A [Cc][Kk][Bb] ([0-9]+) ( $ | [.] ) }x) {
    my $fragment_number = $1;
    return $fragment_number;
  } else {
    croak ('Error: we could not extract the fragment number from the path \'', $fragment_path, '\'.', "\n");
  }
}

sub uc_mizar_name {
    my $name = shift;
    return uc strip_extension ($name);
}

sub fragment_less_than {
    my $a_num = fragment_number ($a);
    my $b_num = fragment_number ($b);
    if ($a_num < $b_num) {
	return -1;
    } elsif ($a_num == $b_num) {
	return 0;
    } else {
	return 1;
    }
}

my %conditions_and_properties_shortcuts
  = ('existence' => 'ex',
     'uniqueness' => 'un',
     'coherence' => 'ch',
     'correctness' => 'cr',
     'abstractness' => 'ab',
     'reflexivity' => 're',
     'irreflexivity' => 'ir',
     'symmetry' => 'sy',
     'asymmetry' => 'as',
     'connectedness' => 'cn',
     'involutiveness' => 'in',
     'projectivity' => 'pr',
     'idempotence' => 'id',
     'commutativity' => 'cm',
     'compatibility' => 'cp',
     'sethood' => 'se',
     'gpattern' => 'gp',
     'jpattern' => 'jp',
     'kpattern' => 'kp',
     'lpattern' => 'lp',
     'gconstructor' => 'gc',
     'jconstructor' => 'jc',
     'kconstructor' => 'kc',
     'lconstructor' => 'lc',
     'rpattern' => 'rp',
     'rconstructor' => 'rc',
     'mpattern' => 'mp',
     'mconstructor' => 'mc',
     'uconstructor' => 'uc',
     'vconstructor' => 'vc',
     'upattern' => 'up',
     'vpattern' => 'vp',
     'deftheorem' => 'dt',
     'kdefiniens' => 'kf',
     'mdefiniens' => 'mf',
     'rdefiniens' => 'rf',
     'vdefiniens' => 'vf');

sub itemize {
    my $self = shift;
    my $target_directory = shift;

    my $article_basename = $self->name ();
    my $local_db = LocalDatabase->new (location => $target_directory);

    my $target_text_subdir = $local_db->get_text_subdir ();
    my $target_prel_subdir = $local_db->get_prel_subdir ();

    # Copy the article miz to the new subdirectory

    my $article_in_target_dir = $self->copy (File::Spec->catdir ($target_directory));

    my $article_evl_in_target_dir = $article_in_target_dir->file_with_extension ('evl');

    # Transform the new miz
    print 'Rewriting the text of ', $article_basename, ': ';

    my $split_stylesheet = Mizar::path_for_stylesheet ('split');
    my $itemize_stylesheet = Mizar::path_for_stylesheet ('itemize');
    my $wsm_stylesheet = Mizar::path_for_stylesheet ('wsm');
    my $extend_evl_stylesheet = Mizar::path_for_stylesheet ('extend-evl');
    my $conditions_and_properties_stylesheet = Mizar::path_for_stylesheet ('conditions-and-properties');
    my $trim_properties_and_conditions_stylesheet = Mizar::path_for_stylesheet ('trim-properties-and-conditions');

    $article_in_target_dir->msmify ()
	or croak ('Error: ', $article_basename, ' cannot be MSMified.');

    print 'done.', "\n";

    my $article_wsx_in_target_dir
	= $article_in_target_dir->file_with_extension ('wsx');
    my $article_split_wsx_in_target_dir
	= $article_in_target_dir->file_with_extension ('wsx.split');
    my $article_itemized_wsx_in_target_dir
	= $article_in_target_dir->file_with_extension ('wsx.split.itemized');

    print 'Split ', $article_basename, ': ';
    apply_stylesheet ($split_stylesheet,
		      $article_wsx_in_target_dir,
		      $article_split_wsx_in_target_dir)
	or
	    croak ('Error: xsltproc did not exit cleanly when applying the split stylesheet at ', $split_stylesheet, ' to ', $article_wsx_in_target_dir, '.', "\n");

    print 'done.', "\n";

    print 'Itemize ', $article_basename, ': ';
    apply_stylesheet ($itemize_stylesheet,
		      $article_split_wsx_in_target_dir,
		      $article_itemized_wsx_in_target_dir)
	or croak ('Error: xsltproc did not exit cleanly when applying the itemize stylesheet at ', $itemize_stylesheet, ' to ', $article_split_wsx_in_target_dir, '.', "\n");

    print 'done.', "\n";

    # Now write the fragments

    my $itemized_article_doc = eval { $xml_parser->parse_file ($article_itemized_wsx_in_target_dir) };
    if (! defined $itemized_article_doc) {
	croak ('Error: the XML in ', $article_itemized_wsx_in_target_dir, ' is not well-formed.', "\n");
    }

    my @fragments = $itemized_article_doc->findnodes ('/Fragments/Text-Proper');

    # Our upper bound: 999.  This is because we may potentially add
    # 2-letter suffixes to the names of our fragments.  We use 'ckb' as
    # the prefix, followed by a number and maybe a two-letter constructor
    # property/correctness condition code, thus, 'ckb50' or 'ckb50ab'.  If
    # there are more than 999 fragments, then we could potentially use
    # names like 'ckb1000ab', which is too long for a Mizar article name
    # (these have to be at most 8 characters long).
    if (scalar @fragments > 999) {
	croak ('Error: because of limitations in Mizar, we cannot itemize articles with more than 999 fragments.');
    }

    # Separate the XML for the fragments into separate files

    foreach my $i (1 .. scalar @fragments) {
	my $fragment = $fragments[$i - 1];
	my $fragment_doc = XML::LibXML::Document->createDocument ();
	$fragment->setAttribute ('original-article', $article_basename);
	$fragment->setAttribute ('fragment-number', $i);
	$fragment_doc->setDocumentElement ($fragment);
	my $fragment_path = "${target_directory}/fragment-${i}.wsx";
	$fragment_doc->toFile ($fragment_path);
    }

    print 'Generating ', scalar @fragments, ' Mizar fragments from ', $article_basename, ': ';

    foreach my $i (1 .. scalar @fragments) {

	print '.';

	my $fragment = $fragments[$i - 1];

	my $fragment_path = "${target_directory}/fragment-${i}.wsx";
	my $fragment_evl = "${target_directory}/fragment-${i}.evl";
	my $fragment_miz = "${target_text_subdir}/ckb${i}.miz";
	my $fragment_xml = "${target_text_subdir}/ckb${i}.xml";
	my $fragment_xml_orig = "${target_text_subdir}/ckb${i}.xml.orig";
	my $fragment_xml_exported = "${target_text_subdir}/ckb${i}.xml.exported";

	# Extend the evl of the initial article by inspecting the contents
	# of the prel subdirectory
	my @new_notations
	    = map { uc_mizar_name ($_) } $local_db->notations_in_prel ();
	my @new_registrations
	    = map { uc_mizar_name ($_) } $local_db->registrations_in_prel ();
	my @new_definitions
	    = map { uc_mizar_name ($_) } $local_db->definitions_in_prel ();
	my @new_theorems
	    = map { uc_mizar_name ($_) } $local_db->theorems_and_deftheorems_in_prel ();
	my @new_schemes
	    = map { uc_mizar_name ($_) } $local_db->schemes_in_prel ();
	my @new_constructors =
	    map { uc_mizar_name ($_) } $local_db->constructors_in_prel ();

	my @sorted_new_notations
	    = sort { fragment_less_than ($a, $b) } @new_notations;
	my @sorted_new_registrations
	    = sort { fragment_less_than ($a, $b) } @new_registrations;
	my @sorted_new_definitions
	    = sort { fragment_less_than ($a, $b) } @new_definitions;
	my @sorted_new_theorems
	    = sort { fragment_less_than ($a, $b) } @new_theorems;
	my @sorted_new_schemes
	    = sort { fragment_less_than ($a, $b) } @new_schemes;
	my @sorted_new_constructors =
	    sort { fragment_less_than ($a, $b) } @new_constructors;

	apply_stylesheet ($extend_evl_stylesheet,
			  $article_evl_in_target_dir,
			  $fragment_evl,
			  {
			      'notations' => list_as_token_string (\@sorted_new_notations),
			      'definitions' => list_as_token_string (\@sorted_new_definitions),
			      'theorems' => list_as_token_string (\@sorted_new_theorems),
			      'registrations' => list_as_token_string (\@sorted_new_registrations),
			      'constructors' => list_as_token_string (\@sorted_new_constructors),
			      'schemes' => list_as_token_string (\@sorted_new_schemes),
			  })
	    or croak ('Error: xsltproc did not exit cleanly when applying the extend-evl stylesheet to ', $article_evl_in_target_dir, '.', "\n");

	# Now render the fragment's XML as a mizar article
	apply_stylesheet ($wsm_stylesheet,
			  $fragment_path,
			  $fragment_miz,
			  { 'evl' => $fragment_evl })
	    or croak ('Error: xsltproc did not exit cleanly when applying the WSM stylesheet to ', $fragment_path, '.', "\n");

	# Now export and transfer the fragment

	my $fragment_basename = "ckb${i}";
	$local_db->accom ($fragment_basename)
	    or croak ('Warning: accom did not exit cleanly when applied to fragment number ', $i, ' of ', $article_basename, '.');

	if ($local_db->verify ($fragment_basename)) {
	    File::Copy::copy ($fragment_xml, $fragment_xml_orig)
		  or croak ('Error: unable to save a copy of ', $fragment_xml, ' to ', $fragment_xml_orig, ': ', $!);
	    if ($local_db->export ($fragment_basename)) {
		File::Copy::copy ($fragment_xml, $fragment_xml_exported)
		      or croak ('Error: unable to copy ', $fragment_xml, ' to ', $fragment_xml_exported, ': ', $!);
		if ($local_db->transfer ($fragment_basename)) {
		    # Restore the non-exported XML
		    File::Copy::copy ($fragment_xml_orig, $fragment_xml)
			  or croak ('Error: unable to restore the original XML at ', $fragment_xml_orig, ' to ', $fragment_xml, ': ', $!);
		} else {
		    carp ('Warning: fragment number ', $i, ' of ', $article_basename, ' is verifiable and exportable, but there was a problem with the call to transfer.');
		}
	    } else {
		carp ('Warning: fragment number ', $i, ' of ', $article_basename, ' is verifiable but not exportable.');
	    }
	} else {
	    carp ('Warning: fragment number ', $i, ' of ', $article_basename, ' is not verifiable.');
	}
    }

    print 'done.', "\n";

    # Now extract all correctness conditions and properties

    print 'Extracting patterns, constructors, properties, and correctness conditions from definitions of ', $article_basename, ': ';

    my @fragment_files = $local_db->articles ();

    print 'done.', "\n";

    foreach my $fragment (@fragment_files) {

	print '.';

	my $fragment_number = fragment_number ($fragment);
	my $fragment_xml = "${target_text_subdir}/${fragment}.xml";
	my $fragment_miz = "${target_text_subdir}/${fragment}.miz";

	my $fragment_article = Article->new (path => $fragment_miz)
	    or croak ('Error creating article for ', $fragment_miz, '.');

	my @correctness_conditions_and_properties
	    = apply_stylesheet ($conditions_and_properties_stylesheet,
				$fragment_xml);

	foreach my $cc_or_p (@correctness_conditions_and_properties) {
	    my $cc_or_p_code = $conditions_and_properties_shortcuts{$cc_or_p};

	    if (! defined $conditions_and_properties_shortcuts{$cc_or_p}) {
		croak ('Error: we are unable to find the short form of the correctness condition/constructor property \'', $cc_or_p, '\'.', "\n");
	    }

	    my $new_prefix = "ckb${fragment_number}${cc_or_p_code}";

	    $fragment_article->copy_with_new_name ($new_prefix);

	    my $new_miz = "${target_text_subdir}/${new_prefix}.miz";
	    my $new_err = "${target_text_subdir}/${new_prefix}.err";
	    my $new_xml = "${target_text_subdir}/${new_prefix}.xml";
	    my $new_xml_tmp = "${target_text_subdir}/${new_prefix}.xml.tmp";
	    ensure_readable_file ($new_miz);
	    ensure_readable_file ($new_err);
	    ensure_readable_file ($new_xml);

	    my $new_xml_orig = "${target_text_subdir}/${new_prefix}.xml.orig";

	    # Save a copy of the old XML
	    File::Copy::copy ($new_xml, $new_xml_orig)
		  or croak ('Error: unable to save a copy of ', $new_xml, ' to ', $new_xml_orig, '.', "\n");

	    apply_stylesheet ($trim_properties_and_conditions_stylesheet,
			      $new_xml,
			      $new_xml_tmp,
			      { 'target-condition-or-property' => $cc_or_p })
		or croak ('Error: something went wrong trimming the property ', $cc_or_p, ' from ', $new_xml, '.');

	    File::Copy::move ($new_xml_tmp, $new_xml)
		or croak ('Error: error moving the temporary XML generated by strippping ', $cc_or_p, ' from fragment ', $fragment_number, '.', "\n");

	    # Now that we've trimmed the XML, minimize to throw away any
	    # spurious toplevel stuff that we don't really need.
	}
    }

    print 'Absolutizing the fragment XMLs...';

    $local_db->absolutize ();

    print 'done.', "\n";

    my $itemized_article = ItemizedArticle->new (local_database => $local_db);

    print 'Rewriting aid attributes...';

    $itemized_article->rewrite_pseudo_fragment_aids ();

    print 'done.', "\n";

    return $itemized_article;

}

sub files {
    my $self = shift;

    my $article_dir = File::Spec->catdir (dirname ($self->get_path ()));
    my $article_base = basename ($self->get_path (), '.miz');

    my @files = ();

    opendir (my $dir_fh, $article_dir)
	or croak ('Error: unable to open the directory ', $article_dir, ' under which ', $self->name (), ' is located.');

    foreach my $file (readdir $dir_fh) {
	if (-f "${article_dir}/${file}") {
	    if ($file =~ /\A ${article_base} [.] .+ \z/) {
		push (@files, $file);
	    }
	}
    }

    closedir $dir_fh
	or croak ('Error: unable to close the directory filehandle!');

    if (wantarray) {
	return @files;
    } else {
	return join (' ', @files);
    }
}

sub copy {
    my $self = shift;
    my $new_path = shift;

    my $current_path = $self->get_path ();
    my $current_dir = dirname ($current_path);
    my $new_path_dir = File::Spec->catdir ($new_path);

    if (! ensure_directory ($new_path_dir)) {
	croak ('Error: the directory name, ', $new_path_dir, ' of the target ', $new_path, ' is not a directory.');
    }

    my @files = $self->files ();

    foreach my $file (@files) {
	my $current_file_path = "${current_dir}/${file}";

	File::Copy::copy ($current_file_path,
			  $new_path_dir)
	      or croak ('Error: unable to copy ', $current_file_path, ' to ', $new_path_dir, '.');
    }

    my $new_path_basename = basename ($new_path, '.miz');

    my $new_path_miz = "${new_path_dir}/${new_path_basename}.miz";

    my $new_article = Article->new (path => $new_path_miz);

    return $new_article;

}

sub copy_with_new_name {
    my $self = shift;
    my $new_basename = shift;

    my $current_path = $self->get_path ();
    my $current_dir = dirname ($current_path);

    my @files = $self->files ();

    foreach my $file (@files) {
	my $current_file_path = "${current_dir}/${file}";
	my $current_file_extension = extension ($file);
	my $new_path = "${current_dir}/${new_basename}.${current_file_extension}";
	File::Copy::copy ($current_file_path,
			  $new_path)
	      or croak ('Error: unable to copy ', $current_file_path, ' to ', $new_path, '.');
    }

    my $new_path_miz = "${current_dir}/${new_basename}.miz";

    my $new_article = Article->new (path => $new_path_miz);

    return $new_article;

}

sub msmify {
    my $self = shift;

    $self->accom ()
	or croak ('Error: unable to accom ', $self->name (), '.');
    $self->wsmparser ()
	or croak ('Error: msmprocessor failed on ', $self->name (), '.');
    $self->msmprocessor ()
	or croak ('Error: msmprocessor failed on ', $self->name (), '.');
    $self->msplit ()
	or croak ('Error: msplit failed on ', $self->name (), '.');

    my $msm_path = $self->file_with_extension ('msm');
    my $tpr_path = $self->file_with_extension ('tpr');

    File::Copy::copy ($msm_path, $tpr_path)
	  or croak ('Error: we are unable to copy ', $msm_path, ' to ', $tpr_path, ': ', $!);

    $self->mglue ()
	or croak ('Error: mglue failed on ', $self->name (), '.');
    $self->wsmparser ()
	or croak ('Error: wsmparser failed on the WSMified ', $self->name (), '.');

    return $self;

}

sub evl_nodes_with_name {
    my $self = shift;
    my $name = shift;

    my $evl = $self->file_with_extension ('evl');

    if (ensure_readable_file ($evl)) {
	my $evl_doc = eval { $xml_parser->parse_file ($evl) };
	if (defined $evl_doc) {
	    my $xpath = 'Environ/Directive[@name = "' . $name . '"]/Ident[@name]';
	    my @nodes
		= $evl_doc->findnodes ($xpath);
	    if (wantarray) {
		return @nodes;
	    } else {
		return join (' ', @nodes);
	    }
	} else {
	    croak ('Error: the .evl file for ', $self->name, ' is not a valid XML file.');
	}
    } else {
	croak ('Error: the .evl file for ', $self->name(), ' does not exist (or is unreadable).');
    }

}

sub evl_idents_under_name {
    my $self = shift;
    my $name = shift;

    my @nodes = $self->evl_nodes_with_name ($name);
    my @names = map { $_->getAttribute ('name') } @nodes;

    if (wantarray) {
	return @names;
    } else {
	return join (' ', @names);
    }

}

sub notations {
    my $self = shift;
    return $self->evl_idents_under_name ('Notations');
}

sub registrations {
    my $self = shift;
    return $self->evl_idents_under_name ('Registrations');
}

sub definitions {
    my $self = shift;
    return $self->evl_idents_under_name ('Definitions');
}

sub theorems {
    my $self = shift;
    return $self->evl_idents_under_name ('Theorems');
}

sub schemes {
    my $self = shift;
    return $self->evl_idents_under_name ('Schemes');
}

sub constructors {
    my $self = shift;
    return $self->evl_idents_under_name ('Constructors');
}

1;
