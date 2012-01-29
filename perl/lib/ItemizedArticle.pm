package ItemizedArticle;

use Moose;
use Carp qw(croak);
use File::Spec;
use Data::Dumper;
use File::Basename qw(basename);
use Regexp::DefaultFlags;
use XML::LibXML;

# Local stuff
use Utils qw(ensure_directory strip_extension ensure_readable_file);

has 'local_database' => (
    is => 'ro',
    isa => 'LocalDatabase',
    reader => 'get_local_database',
    required => 1,
);

has 'item_to_fragment_table' => (
    is => 'rw',
    traits => [qw(Hash)],
    isa => 'HashRef',
    reader => 'get_item_to_fragment_table',
    writer => '_set_item_to_fragment_table',
);

has 'fragment_to_item_table' => (
    is => 'rw',
    traits => [qw(Hash)],
    isa => 'HashRef',
    reader => 'get_fragment_to_item_table',
    writer => '_set_fragment_to_item_table',
);

has 'article_name' => (
    is => 'rw',
    isa => 'Str',
    reader => 'get_article_name',
    writer => 'set_article_name',
);

has 'fragment_to_constructor_table' => (
    is => 'rw',
    traits => [qw(Hash)],
    isa => 'HashRef',
    reader => 'get_fragment_to_constructor_table',
    writer => '_set_fragment_to_constructor_table',
);

has 'fragment_to_definientia_table' => (
    is => 'rw',
    traits => [qw(Hash)],
    isa => 'HashRef',
    reader => 'get_fragment_to_definientia_table',
    writer => '_set_fragment_to_definientia_table',
);

sub _set_article_name {

    my $self = shift;

    # Extract the article from which the local database was itemized
    my $local_db = $self->get_local_database ();

    if (! defined $local_db) {
	croak ('Error: the mandatory local-database argument seems to be missing!');
    }

    my $local_db_location = $local_db->get_location ();

    if (! ensure_directory ($local_db_location)) {
	croak ('Error: the location where the local database is stored (', $local_db_location, ') does not seem to exist (as a directory).');
    }

    my $local_db_location_as_dir = File::Spec->catdir ($local_db_location);
    my @candidates = glob "${local_db_location_as_dir}*.miz";

    if (scalar @candidates == 0) {
	croak ('Error: we found no .miz files under ', $local_db_location_as_dir, '.');
    }

    if (scalar @candidates > 1) {
	croak ('Error: we found multiple .miz files under ', $local_db_location_as_dir, '; will the real itemized article please stand up?');
    }

    my $itemized_article_miz = $candidates[0];

    my $article_name = basename ($itemized_article_miz, '.miz');

    $self->set_article_name ($article_name);

}

sub BUILD {

    my $self = shift;

    # warn 'Local basebase: ', Dumper ($self->get_local_database ());

    my %empty_table = ();
    $self->_set_item_to_fragment_table ( \%empty_table );

    $self->_set_article_name ();

    my %fragment_to_constructor_table = ();
    $self->_set_fragment_to_constructor_table (\%fragment_to_constructor_table);

    my %fragment_to_definientia_table = ();
    $self->_set_fragment_to_definientia_table (\%fragment_to_definientia_table);

    $self->load_item_to_fragment_table ();

    $self->load_fragment_to_item_table ();

}

my $xml_parser = XML::LibXML->new (suppress_warnings => 1, # quiet, please
				   suppress_errors => 1);

sub fragment_number {
    my $fragment = shift;
    if ($fragment =~ / ckb ([0-9]+) /) {
	return $1;
    } else {
	croak ('Error: unable to make sense of the fragment \'', $fragment, '\'.');
    }
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

my %conditions
    = ('ex' => 'existence',
       'un' => 'uniqueness',
       'ch' => 'coherence',
       'cr' => 'correctness',
       'ab' => 'abstractness',
       're' => 'reflexivity',
       'ir' => 'irreflexivity',
       'sy' => 'symmetry',
       'as' => 'asymmetry',
       'cn' => 'connectedness',
       'in' => 'involutiveness',
       'pr' => 'projectivity',
       'id' => 'idempotence',
       'cm' => 'commutativity',
       'cp' => 'compatibility',
       'se' => 'sethood',
       'vc' => 'vconstructor',
       'rc' => 'rconstructor',
       'kc' => 'kconstructor',
       'kf' => 'kdefiniens',
       'mf' => 'mdefiniens',
       'rf' => 'rdefiniens',
       'vf' => 'vdefiniens',
       'kp' => 'kpattern',
       'rp' => 'rpattern',
       'mp' => 'mpattern',
       'vp' => 'vpattern',
       'dt' => 'deftheorem');

my %code_of_property = ();
foreach my $condition (keys %conditions) {
    my $property = $conditions{$condition};
    $code_of_property{$property} = $condition;
}

sub load_item_to_fragment_table {

    my $self = shift;

    $self->load_constructors ();
    $self->load_definientia ();
    $self->load_constructor_properties ();
    $self->load_correctness_conditions ();
    $self->load_notations ();
    $self->load_deftheorems ();
    $self->load_schemes ();
    $self->load_theorems ();
    $self->load_lemmas ();
    $self->load_reductions ();
    $self->load_clusters ();
    $self->load_identifications ();

    return 1;

}

sub load_fragment_to_item_table {

    my $self = shift;

    my %item_to_fragment_table = %{$self->get_item_to_fragment_table};
    my %fragment_to_item_table = ();

    foreach my $fragment (values %item_to_fragment_table) {
	my @items = ();
	foreach my $item (keys %item_to_fragment_table) {
	    if ($fragment eq $item_to_fragment_table{$item}) {
		push (@items, $item);
	    }
	}
	$fragment_to_item_table{$fragment} = \@items;
    }

    # warn 'Setting the fragment-to-item table to:', Dumper (%fragment_to_item_table);

    $self->_set_fragment_to_item_table (\%fragment_to_item_table);

}

sub items {
    my $self = shift;
    return keys %{$self->get_item_to_fragment_table ()};
}

sub fragments {
    my $self = shift;
    return values %{$self->get_item_to_fragment_table ()};
}

sub load_properties_of_constructors_of_kind {
    my $self = shift;
    my $kind = shift;

    my $kind_uc = uc $kind;

    my $local_db = $self->get_local_database ();
    my $prel_subdir = $local_db->get_prel_subdir ();
    my $article_name = $self->get_article_name ();
    my %item_to_fragment_table = %{$self->get_item_to_fragment_table ()};

    my @dcos = $local_db->constructors_in_prel_of_kind ($kind);
    my @dcos_no_extension = map { strip_extension ($_) } @dcos;
    my @sorted_dcos
	= sort { fragment_less_than ($a, $b) } @dcos_no_extension;

    foreach my $i (1 .. scalar @sorted_dcos) {
	my $dco = $sorted_dcos[$i - 1];
	my $fragment_number = fragment_number ($dco);
	my $fragment = "${article_name}:fragment:${fragment_number}";
	my $dco_path = "${prel_subdir}/${dco}.dco";

	if (! ensure_readable_file ($dco_path)) {
	    croak ('Error: the dco file ', $dco, ' does not exist at the expected location (', $dco_path, '), or is unreadable.');
	}

	my $dco_doc = eval { $xml_parser->parse_file ($dco_path) };

	if (! defined $dco_doc) {
	    croak ('Error: the .dco file at ', $dco_path, ' is not a well-formed XML file.');
	}

	my $xpath = '/Constructors/Constructor[@kind = "' . $kind_uc . '"]/Properties/*';
	my @property_nodes = $dco_doc->findnodes ($xpath);
	foreach my $property_node (@property_nodes) {
	    my $property = lc $property_node->nodeName ();

	    if ($property eq 'antisymmetry') {
		$property = 'asymmetry';
	    }

	    my $item = "${article_name}:${kind}constructor:${i}[${property}]";

	    my $property_code = $code_of_property{$property};

	    if (defined $property_code) {
		my $pseudo_fragment = "${fragment}[$property_code]";
		$item_to_fragment_table{$item} = $pseudo_fragment;
	    } else {
		croak ('Error: what is the short form of \'', $property, '\'?');
	    }


	}
    }

    $self->_set_item_to_fragment_table (\%item_to_fragment_table);

    return 1;

}

sub load_constructor_properties {
    my $self = shift;

    foreach my $kind ('k', 'm', 'r', 'v') {
	$self->load_properties_of_constructors_of_kind ($kind);
    }

    return $self->get_item_to_fragment_table ();

}

sub load_constructors_of_kind {
    my $self = shift;
    my $kind = shift;

    my $local_db = $self->get_local_database ();

    my $article_name = $self->get_article_name ();

    my %item_to_fragment_table = %{$self->get_item_to_fragment_table ()};
    my %fragment_to_constructor_table = %{$self->get_fragment_to_constructor_table ()};

    my @dcos = $local_db->constructors_in_prel_of_kind ($kind);
    my @dcos_no_extension = map { strip_extension ($_) } @dcos;
    my @sorted_dcos
	= sort { fragment_less_than ($a, $b) } @dcos_no_extension;

    foreach my $i (1 .. scalar @sorted_dcos) {
	my $fragment_of_ccluster = $sorted_dcos[$i - 1];
	my $fragment_number = fragment_number ($fragment_of_ccluster);
	my $item = "${article_name}:${kind}constructor:${i}";
	my $fragment = "${article_name}:fragment:${fragment_number}";
	$item_to_fragment_table{$item} = $fragment;

	# Record that this fragment number generates this constructor.
	# Later, when computing correctness conditions, we'll need
	# this information.

	if (defined $fragment_to_constructor_table{$fragment_number}) {
	    my @other_constructors
		= @{$fragment_to_constructor_table{$fragment_number}};
	    push (@other_constructors, $item);
	} else {
	    $fragment_to_constructor_table{$fragment_number} = [$item];
	}

    }

    $self->_set_item_to_fragment_table (\%item_to_fragment_table);
    $self->_set_fragment_to_constructor_table (\%fragment_to_constructor_table);

    return 1;

}

sub load_constructors {
    my $self = shift;

    foreach my $kind ('g', 'k', 'l', 'm', 'r', 'u', 'v') {
	$self->load_constructors_of_kind ($kind);
    }

    return $self->get_item_to_fragment_table ();
}

sub load_patterns_of_kind {
    my $self = shift;
    my $kind = shift;

    my $local_db = $self->get_local_database ();
    my $article_name = $self->get_article_name ();
    my %item_to_fragment_table = %{$self->get_item_to_fragment_table ()};
    my @dnos = $local_db->patterns_in_prel_of_kind ($kind);
    my @dnos_no_extension = map { strip_extension ($_) } @dnos;
    my @sorted_dnos
	= sort { fragment_less_than ($a, $b) } @dnos_no_extension;

    foreach my $i (1 .. scalar @sorted_dnos) {
	my $fragment = $sorted_dnos[$i - 1];
	my $fragment_number = fragment_number ($fragment);
	my $item = "${article_name}:${kind}pattern:${i}";
	$item_to_fragment_table{$item} = "${article_name}:fragment:${fragment_number}";
    }

    $self->_set_item_to_fragment_table (\%item_to_fragment_table);

    return 1;

}

sub load_notations {
    my $self = shift;

    foreach my $kind ('g', 'j', 'k', 'l', 'm', 'r', 'u', 'v') {
	$self->load_patterns_of_kind ($kind);
    }

    return 1;
}

sub load_definientia_of_kind {
    my $self = shift;
    my $kind = shift;

    my $article_name = $self->get_article_name ();
    my $local_db = $self->get_local_database ();
    my %item_to_fragment_table = %{$self->get_item_to_fragment_table};
    my %fragment_to_definientia_table = %{$self->get_fragment_to_definientia_table ()};

    my @definientia = $local_db->definientia_in_prel_of_kind ($kind);
    my @definientia_no_extension = map { strip_extension ($_) } @definientia;
    my @sorted_definientia
	= sort { fragment_less_than ($a, $b) } @definientia_no_extension;

    foreach my $i (1 .. scalar @sorted_definientia) {
	my $fragment = $sorted_definientia[$i - 1];
	my $fragment_number = fragment_number ($fragment);
	my $item = "${article_name}:${kind}definiens:${i}";
	$item_to_fragment_table{$item} = "${article_name}:fragment:${fragment_number}";

	# Record that this fragment number generates this definiens.
	# Later, when computing correctness conditions, we may need
	# this information.

	if (defined $fragment_to_definientia_table{$fragment_number}) {
	    my @other_definiens
		= @{$fragment_to_definientia_table{$fragment_number}};
	    push (@other_definiens, $item);
	} else {
	    $fragment_to_definientia_table{$fragment_number} = [$item];
	}

    }

    $self->_set_fragment_to_definientia_table (\%fragment_to_definientia_table);
    $self->_set_item_to_fragment_table (\%item_to_fragment_table);

}

sub load_definientia {
    my $self = shift;

    foreach my $kind ('k', 'm', 'r', 'v') {
	$self->load_definientia_of_kind ($kind);
    }

    return 1;
}

sub load_correctness_conditions {

    my $self = shift;

    $self->load_existence_conditions ();
    $self->load_function_uniqueness_conditions ();
    $self->load_constructor_coherence_conditions ();
    $self->load_redefined_constructor_compatibility_conditions ();

}

sub items_generated_by_fragment {
    my $self = shift;
    my $fragment = shift;

    my $fragment_to_item_table_ref = $self->get_fragment_to_item_table ();

    if (! defined $fragment_to_item_table_ref) {
	croak ('Error: the fragment-to-item table seems not to be loaded.');
    }

    my %fragment_to_item_table = %{$fragment_to_item_table_ref};

    my @items = defined $fragment_to_item_table{$fragment} ? @{$fragment_to_item_table{$fragment}} : ();

    if (wantarray) {
	return @items;
    } else {
	return join (' ', @items);
    }
}

sub load_existence_conditions {
    my $self = shift;

    my $article_name = $self->get_article_name ();
    my %item_to_fragment_table = %{$self->get_item_to_fragment_table ()};
    my %fragment_to_constructor_table = %{$self->get_fragment_to_constructor_table ()};
    my $local_db = $self->get_local_database ();
    my $local_db_text_subdir = File::Spec->catdir ($local_db->get_text_subdir ());

    my @conditions = glob "${local_db_text_subdir}/*ex.miz";

    foreach my $file (@conditions) {
	if ($file =~ /ckb ([0-9]+) ex /) {
	    my $fragment_number = $1;

	    my $constructors_ref = $fragment_to_constructor_table{$fragment_number};

	    if (! defined $constructors_ref) {
		croak ('Error: we know of no constructors generated by fragment ', $fragment_number);
	    }

	    my @generated_constructors = @{$constructors_ref};

	    my @existence_constructors
		= grep { / : (k | m) constructor  : [0-9]+ \z/ } @generated_constructors;

	    if (scalar @existence_constructors == 0) {
		carp ('Warning: fragment ', $fragment_number, ' generated no mode constructors and no functor constructors.  How is it possible that there is an existence pseudo-fragment associated with this fragment?');
	    }

	    foreach my $constructor (@existence_constructors) {
		my $item = "${constructor}[existence]";
		my $pseudo_fragment = "${article_name}:fragment:${fragment_number}[ex]";
		$item_to_fragment_table{$item} = $pseudo_fragment;
	    }

	}
    }

    $self->_set_item_to_fragment_table (\%item_to_fragment_table);

    return 1;

}

sub load_function_uniqueness_conditions {
    my $self = shift;

    my $article_name = $self->get_article_name ();
    my %item_to_fragment_table = %{$self->get_item_to_fragment_table ()};
    my %fragment_to_constructor_table = %{$self->get_fragment_to_constructor_table ()};
    my $local_db = $self->get_local_database ();
    my $local_db_text_subdir = File::Spec->catdir ($local_db->get_text_subdir ());

    my @conditions = glob "${local_db_text_subdir}/*un.miz";

    foreach my $file (@conditions) {
	if ($file =~ /ckb ([0-9]+) un /) {
	    my $fragment_number = $1;

	    my $constructors_ref = $fragment_to_constructor_table{$fragment_number};

	    if (! defined $constructors_ref) {
		croak ('Error: we know of no constructors generated by fragment ', $fragment_number);
	    }

	    my @generated_constructors = @{$constructors_ref};

	    my @functor_constructors
		= grep { / : kconstructor  : [0-9]+ \z/ } @generated_constructors;

	    if (scalar @functor_constructors == 0) {
		carp ('Warning: fragment ', $fragment_number, ' generated no functor constructors.  How is it possible that there is an existence pseudo-fragment associated with this fragment?');
	    }

	    foreach my $constructor (@functor_constructors) {
		my $item = "${constructor}[uniqueness]";
		my $pseudo_fragment = "${article_name}:fragment:${fragment_number}[un]";
		$item_to_fragment_table{$item} = $pseudo_fragment;
	    }

	}
    }

    $self->_set_item_to_fragment_table (\%item_to_fragment_table);

    return 1;

}

sub load_constructor_coherence_conditions {
    my $self = shift;

    my $article_name = $self->get_article_name ();
    my %item_to_fragment_table = %{$self->get_item_to_fragment_table ()};
    my %fragment_to_constructor_table = %{$self->get_fragment_to_constructor_table ()};
    my $local_db = $self->get_local_database ();
    my $local_db_text_subdir = File::Spec->catdir ($local_db->get_text_subdir ());

    my @conditions = glob "${local_db_text_subdir}/*ch.miz";

    foreach my $file (@conditions) {
	if ($file =~ /ckb ([0-9]+) ch /) {
	    my $fragment_number = $1;

	    my $constructors_ref = $fragment_to_constructor_table{$fragment_number};

	    if (! defined $constructors_ref) {
		croak ('Error: we know of no constructors generated by fragment ', $fragment_number);
	    }

	    my @generated_constructors = @{$constructors_ref};

	    if (scalar @generated_constructors == 0) {
		carp ('Warning: fragment ', $fragment_number, ' generated no constructors.  How is it possible that there is a coherence pseudo-fragment associated with this fragment?');
	    }

	    foreach my $constructor (@generated_constructors) {
		my $item = "${constructor}[coherence]";
		my $pseudo_fragment = "${article_name}:fragment:${fragment_number}[ch]";
		$item_to_fragment_table{$item} = $pseudo_fragment;
	    }

	}
    }

    $self->_set_item_to_fragment_table (\%item_to_fragment_table);

    return 1;

}

sub load_redefined_constructor_compatibility_conditions {
    my $self = shift;

    my $article_name = $self->get_article_name ();
    my %item_to_fragment_table = %{$self->get_item_to_fragment_table ()};
    my %fragment_to_definiens_table = %{$self->get_fragment_to_definientia_table ()};
    my $local_db = $self->get_local_database ();
    my $local_db_text_subdir = File::Spec->catdir ($local_db->get_text_subdir ());

    my @conditions = glob "${local_db_text_subdir}/*cp.miz";

    foreach my $file (@conditions) {
	if ($file =~ /ckb ([0-9]+) cp /) {
	    my $fragment_number = $1;

	    my $definiens_ref = $fragment_to_definiens_table{$fragment_number};

	    if (! defined $definiens_ref) {
		croak ('Error: we know of no definientia generated by fragment ', $fragment_number);
	    }

	    my @generated_definientia = @{$definiens_ref};

	    if (scalar @generated_definientia == 0) {
		croak ('Warning: fragment ', $fragment_number, ' generated no definiens items.  How is it possible that there is a compatibility pseudo-fragment associated with this fragment?');
	    }

	    if (scalar @generated_definientia > 1) {
		croak ('Warning: fragment ', $fragment_number, ' generated multiple definiens items.  Which one should we choose?');
	    }

	    my $definiens = $generated_definientia[0];
	    my $item = "${definiens}[compatibility]";
	    my $pseudo_fragment = "${article_name}:fragment:${fragment_number}[cp]";
	    $item_to_fragment_table{$item} = $pseudo_fragment;

	}
    }

    $self->_set_item_to_fragment_table (\%item_to_fragment_table);

    return 1;

}


sub load_deftheorems {
    my $self = shift;

    my $local_db = $self->get_local_database ();
    my $article_name = $self->get_article_name ();
    my %item_to_fragment_table = %{$self->get_item_to_fragment_table ()};

    my @theorems = $local_db->deftheorems_in_prel ();
    my @theorems_no_extension = map { strip_extension ($_) } @theorems;
    my @sorted_theorems = sort { fragment_less_than ($a, $b) } @theorems_no_extension;

    foreach my $i (1 .. scalar @sorted_theorems) {
	my $fragment_of_theorem = $sorted_theorems[$i - 1];
	my $fragment_number = fragment_number ($fragment_of_theorem);
	my $item = "${article_name}:deftheorem:${i}";
	$item_to_fragment_table{$item} = "${article_name}:fragment:${fragment_number}";
    }

    $self->_set_item_to_fragment_table (\%item_to_fragment_table);

    return 1;

}

# sub fragment_number {
#     my $fragment = shift;
#     if ($fragment =~ / ckb ([0-9]+) /) {
# 	return $1;
#     } else {
# 	croak ('Error: unable to make sense of the fragment \'', $fragment, '\'.');
#     }
# }

# sub fragment_less_than {
#     my $a_num = fragment_number ($a);
#     my $b_num = fragment_number ($b);
#     if ($a_num < $b_num) {
# 	return -1;
#     } elsif ($a_num == $b_num) {
# 	return 0;
#     } else {
# 	return 1;
#     }
# }

sub load_schemes {
    my $self = shift;

    my $local_db = $self->get_local_database ();
    my %item_to_fragment_table = %{$self->get_item_to_fragment_table ()};
    my $article_name = $self->get_article_name ();

    my @schemes = $local_db->schemes_in_prel ();
    my @schemes_no_extension = map { strip_extension ($_) } @schemes;
    my @sorted_schemes = sort { fragment_less_than ($a, $b) } @schemes_no_extension;

    foreach my $i (1 .. scalar @sorted_schemes) {
	my $fragment_of_scheme = $sorted_schemes[$i - 1];
	my $fragment_number = fragment_number ($fragment_of_scheme);
	my $item = "${article_name}:scheme:${i}";
	$item_to_fragment_table{$item} = "${article_name}:fragment:${fragment_number}";
    }

    $self->_set_item_to_fragment_table (\%item_to_fragment_table);

    return 1;

}

sub load_reductions {
    my $self = shift;


    my $local_db = $self->get_local_database ();
    my %item_to_fragment_table = %{$self->get_item_to_fragment_table ()};
    my $article_name = $self->get_article_name ();

    my @reductions = $local_db->reductions_in_prel ();
    my @reductions_no_extension = map { strip_extension ($_) } @reductions;
    my @sorted_reductions
	= sort { fragment_less_than ($a, $b) } @reductions_no_extension;

    foreach my $i (1 .. scalar @sorted_reductions) {
	my $fragment_of_reduction = $sorted_reductions[$i - 1];
	my $fragment_number = fragment_number ($fragment_of_reduction);
	my $item = "${article_name}:reduction:${i}";
	$item_to_fragment_table{$item} = "${article_name}:fragment:${fragment_number}";
    }

    $self->_set_item_to_fragment_table (\%item_to_fragment_table);

    return 1;

}

# sub print_conditional_clusters {

#     my @cclusters = $local_db->conditional_clusters_in_prel ();
#     my @cclusters_no_extension = map { strip_extension ($_) } @cclusters;
#     my @sorted_cclusters
# 	= sort { fragment_less_than ($a, $b) } @cclusters_no_extension;

#     foreach my $i (1 .. scalar @sorted_cclusters) {
# 	my $fragment_of_ccluster = $sorted_cclusters[$i - 1];
# 	my $fragment_number = fragment_number ($fragment_of_ccluster);
# 	print $article_basename, ':', 'ccluster', ':', $i, ' => ', $article_basename, ':', 'fragment', ':', $fragment_number, "\n";
#     }

# }

# sub print_functorial_clusters {

#     my @fclusters = $local_db->functorial_clusters_in_prel ();
#     my @fclusters_no_extension = map { strip_extension ($_) } @fclusters;
#     my @sorted_fclusters
# 	= sort { fragment_less_than ($a, $b) } @fclusters_no_extension;

#     foreach my $i (1 .. scalar @sorted_fclusters) {
# 	my $fragment_of_fcluster = $sorted_fclusters[$i - 1];
# 	my $fragment_number = fragment_number ($fragment_of_fcluster);
# 	print $article_basename, ':', 'fcluster', ':', $i, ' => ', $article_basename, ':', 'fragment', ':', $fragment_number, "\n";
#     }

# }

# sub print_existential_clusters {

#     my @rclusters = $local_db->existential_clusters_in_prel ();
#     my @rclusters_no_extension = map { strip_extension ($_) } @rclusters;
#     my @sorted_rclusters
# 	= sort { fragment_less_than ($a, $b) } @rclusters_no_extension;

#     foreach my $i (1 .. scalar @sorted_rclusters) {
# 	my $fragment_of_rcluster = $sorted_rclusters[$i - 1];
# 	my $fragment_number = fragment_number ($fragment_of_rcluster);
# 	print $article_basename, ':', 'rcluster', ':', $i, ' => ', $article_basename, ':', 'fragment', ':', $fragment_number, "\n";
#     }

# }

sub load_clusters_of_kind {
    my $self = shift;
    my $kind = shift;

    my $local_db = $self->get_local_database ();
    my %item_to_fragment_table = %{$self->get_item_to_fragment_table ()};
    my $article_name = $self->get_article_name ();

    my @clusters = $local_db->clusters_in_prel_of_kind ($kind);
    my @clusters_no_extension = map { strip_extension ($_) } @clusters;
    my @sorted_clusters
	= sort { fragment_less_than ($a, $b) } @clusters_no_extension;

    foreach my $i (1 .. scalar @sorted_clusters) {
	my $fragment_of_cluster = $sorted_clusters[$i - 1];
	my $fragment_number = fragment_number ($fragment_of_cluster);
	my $item = "${article_name}:${kind}cluster:${i}";
	$item_to_fragment_table{$item} = "${article_name}:fragment:${fragment_number}";
    }

    $self->_set_item_to_fragment_table (\%item_to_fragment_table);

    return 1;

}

sub load_clusters {
    my $self = shift;

    foreach my $kind ('c', 'f', 'r') {
	$self->load_clusters_of_kind ($kind);
    }

    return 1;
}

sub load_theorems {
    my $self = shift;

    my $local_db = $self->get_local_database ();
    my %item_to_fragment_table = %{$self->get_item_to_fragment_table ()};
    my $article_name = $self->get_article_name ();

    my @theorems = $local_db->theorems_in_prel ();
    my @theorems_no_extension = map { strip_extension ($_) } @theorems;
    my @sorted_theorems = sort { fragment_less_than ($a, $b) } @theorems_no_extension;

    foreach my $i (1 .. scalar @sorted_theorems) {
	my $fragment_of_theorem = $sorted_theorems[$i - 1];
	my $fragment_number = fragment_number ($fragment_of_theorem);
	my $item = "${article_name}:theorem:${i}";
	$item_to_fragment_table{$item} = "${article_name}:fragment:${fragment_number}";
    }

    $self->_set_item_to_fragment_table (\%item_to_fragment_table);

    return 1;

}

sub load_identifications_of_kind {

    my $self = shift;
    my $kind = shift;

    my $local_db = $self->get_local_database ();
    my %item_to_fragment_table = %{$self->get_item_to_fragment_table ()};
    my $article_name = $self->get_article_name ();

    my @identifications = $local_db->clusters_in_prel_of_kind ($kind);
    my @identifications_no_extension = map { strip_extension ($_) } @identifications;
    my @sorted_identifications
	= sort { fragment_less_than ($a, $b) } @identifications_no_extension;

    foreach my $i (1 .. scalar @sorted_identifications) {
	my $fragment = $sorted_identifications[$i - 1];
	my $fragment_number = fragment_number ($fragment);
	my $item = "${article_name}:${kind}identification:${i}";
	$item_to_fragment_table{$item} = "${article_name}:fragment:${fragment_number}";
    }

    $self->_set_item_to_fragment_table (\%item_to_fragment_table);

    return 1;

}

sub load_identifications {
    my $self = shift;

    foreach my $kind ('k') {
	$self->load_identifications_of_kind ($kind);
    }

    return 1;
}

sub load_lemmas {

    # Nothing yet

    return 1;

}

# print_constructors ();
# print_constructor_properties ();
# print_correctness_conditions ();
# print_notations ();
# print_definientia ();
# print_deftheorems ();
# print_schemes ();
# print_reductions ();
# print_clusters ();
# print_identifications ();
# print_theorems ();
# print_lemmas ();


1;
__END__
