package ItemizedArticle;

use Moose;
use Carp qw(croak carp confess);
use File::Spec;
use Data::Dumper;
use File::Basename qw(basename);
use Regexp::DefaultFlags;
use XML::LibXML;
use charnames qw(:full);
use Readonly;

# Local stuff
use Utils qw(strip_extension ensure_readable_file);
use Mizar;
use Xsltproc qw(apply_stylesheet);
use LocalDatabase;

has 'local_database' => (
    is => 'rw',
    isa => 'LocalDatabase',
    reader => 'get_local_database',
    writer => '_set_local_database',
);

has 'location' => (
    is => 'rw',
    isa => 'Str',
    reader => 'get_location',
    writer => '_set_location',
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

has 'fragment_of_lemma_table' => (
    is => 'rw',
    traits => [qw(Hash)],
    isa => 'HashRef',
    reader => 'get_fragment_of_lemma_table',
    writer => '_set_fragment_of_lemma_table',
);

has 'all_items' => (
    is => 'rw',
    traits => [qw(Array)],
    isa => 'ArrayRef',
    reader => 'get_all_items',
    writer => '_set_all_items',
);

has 'all_fragments' => (
    is => 'rw',
    traits => [qw(Array)],
    isa => 'ArrayRef',
    reader => 'get_all_fragments',
    writer => '_set_all_fragments',
);

has 'stylesheet_home' => (
    is => 'ro',
    isa => 'Str',
    reader => 'get_stylesheet_home',
    required => 1,
);

has 'script_home' => (
    is => 'ro',
    isa => 'Str',
    reader => 'get_script_home',
    required => 1,
);

sub ensure_appropriate_directory_structure {
    my $dir = shift;
    return LocalDatabase::dir_has_local_db_structure ($dir);
}

sub _set_article_name {

    my $self = shift;

    # Extract the article from which the local database was itemized
    my $local_db = undef;
    my $location = undef;
    my $stylesheet_home = $self->get_stylesheet_home ();
    my $script_home = $self->get_script_home ();

    if (defined $self->get_local_database () && defined $self->get_location ()) {
	croak ('Error: please use either the local_database or location parameters when creating an ItemizedArticle, but not both.');
    } elsif (defined $self->get_local_database ()) {
	$local_db = $self->get_local_database ();
	$location = $local_db->get_location ();
	$self->_set_location ($location);
    } elsif (defined $self->get_location ()) {
	$location = $self->get_location ();
	$local_db = LocalDatabase->new (location => $location,
				        stylesheet_home => $stylesheet_home,
				        script_home => $script_home);
	$self->_set_local_database ($local_db);
    } else {
	croak ('Error: either a local database or a location must be provided when creating an ItemizedArticle.');
    }

    if (! -d $location) {
	croak ('Error: the location where the local database is stored (', $location, ') does not seem to exist (as a directory).');
    }

    my $location_as_dir = File::Spec->catdir ($location);
    my @candidates = glob "${location_as_dir}/*.miz";

    if (scalar @candidates == 0) {
	croak ('Error: we found no .miz files under ', $location_as_dir, '.');
    }

    if (scalar @candidates > 1) {
	croak ('Error: we found multiple .miz files under ', $location_as_dir, '; will the real itemized article please stand up?');
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

    my %fragment_of_lemma_table = ();
    $self->_set_fragment_of_lemma_table (\%fragment_of_lemma_table);

    $self->load_item_to_fragment_table ();

    $self->load_fragment_to_item_table ();

    my %item_to_fragment_table = %{$self->get_item_to_fragment_table ()};

    my %items_table = ();
    my %fragments_table = ();
    while ((my $k, my $v) = each %item_to_fragment_table) {
	$items_table{$k} = 0;
	$fragments_table{$v} = 0;
    }

    my @items = keys %items_table;
    my @fragments = keys %fragments_table;
    $self->_set_all_items (\@items);
    $self->_set_all_fragments (\@fragments);

    my $sheet_home = $self->get_stylesheet_home ();
    if (! -d $sheet_home) {
	croak ('Error: the supplied path (', $sheet_home, ') is not a directory.');
    }

    return $self;

}

sub path_for_stylesheet {
    my $self = shift;
    my $sheet = shift;
    my $stylesheet_home = $self->get_stylesheet_home ();
    my $sheet_path = "${stylesheet_home}/${sheet}.xsl";
    if (ensure_readable_file ($sheet_path)) {
	return $sheet_path;
    } else {
	croak 'Error: the ', $sheet, ' stylesheet could not be found at the expected location', "\n", "\n", $sheet_path, "\n";
    }
}

my $xml_parser = XML::LibXML->new (suppress_warnings => 1, # quiet, please
				   suppress_errors => 1);

sub get_text_subdir {
    my $self = shift;
    my $local_db = $self->get_local_database ();
    return $local_db->get_text_subdir ();
}

sub text_for_fragment {
    my $self = shift;
    my $fragment = shift;

    my $fragment_article_name = "ckb${fragment}";
    my $local_db = $self->get_local_database ();

    if ($local_db->has_article ($fragment_article_name)) {
	return $local_db->text_of_article ($fragment_article_name);
    } else {
	my $location = $self->get_location ();
	croak ('Error: there is no fragment ', $fragment, ' under ', $location, '.');
    }

}

sub fragment_number {
    my $fragment = shift;
    if ($fragment =~ / ckb ([0-9]+) /) {
	return $1;
    } elsif ($fragment =~ / : fragment : ([0-9]+) /) {
	return $1;
    } else {
	croak ('Error: unable to make sense of the fragment \'', $fragment, '\'.');
    }
}

sub fragment_less_than {
    my $self = shift;
    my $a = shift;
    my $b = shift;
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

sub fragment_for_item {
    my $self = shift;
    my $item = shift;

    my $article = $self->get_article_name ();
    my %item_to_fragment_table = %{$self->get_item_to_fragment_table ()};

    my $fragment = undef;

    if (defined $item_to_fragment_table{$item}) {
	$fragment = $item_to_fragment_table{$item};

	if (! defined $fragment) {
	    croak 'Error: the entry in the item-to-fragment table of ', $article, ' for \'', $item, '\' is undefined.';
	}

    } elsif ($item =~ / (.+) [[] .+ []] \z /) {
	my $item_sans_condition = $1;
	return $self->fragment_for_item ($item_sans_condition);
    } else {
	my @keys = keys %item_to_fragment_table;
	croak 'Error: we cannot find \'', $item, '\' in the item-to-fragment table for ', $article, ', nor could we resolve this item by stripping away a condition (e.g., existence, uniqueness).  The keys of the table are:', "\n", Dumper (@keys);
    }

    return $fragment;

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
    $self->load_lemmas ();
    $self->load_theorems ();
    $self->load_reductions ();
    $self->load_clusters ();
    $self->load_identifications ();

    return 1;

}

sub load_fragment_to_item_table {

    my $self = shift;

    my $article_name = $self->get_article_name ();
    my %item_to_fragment_table = %{$self->get_item_to_fragment_table};
    my %fragment_to_item_table = ();

    foreach my $fragment (values %item_to_fragment_table) {
      my @items = ();
      foreach my $item (keys %item_to_fragment_table) {
	my $other_fragment = $item_to_fragment_table{$item};
	if ($other_fragment eq $fragment) {
	  push (@items, $item);
	}
	$fragment_to_item_table{$fragment} = \@items;
      }
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
	= sort { $self->fragment_less_than ($a, $b) } @dcos_no_extension;

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
		$item_to_fragment_table{$item} = "${fragment}[$property_code]";
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
    my $text_dir = $self->get_text_subdir ();
    my $article_name = $self->get_article_name ();

    my %item_to_fragment_table = %{$self->get_item_to_fragment_table ()};
    my %fragment_to_constructor_table = %{$self->get_fragment_to_constructor_table ()};

    my @dcos = $local_db->constructors_in_prel_of_kind ($kind);
    my @dcos_no_extension = map { strip_extension ($_) } @dcos;
    my @sorted_dcos
	= sort { $self->fragment_less_than ($a, $b) } @dcos_no_extension;

    foreach my $i (1 .. scalar @sorted_dcos) {
	my $fragment_of_ccluster = $sorted_dcos[$i - 1];
	my $fragment_number = fragment_number ($fragment_of_ccluster);
	my $item = "${article_name}:${kind}constructor:${i}";

	$item_to_fragment_table{$item}
	    = "${article_name}:fragment:${fragment_number}[${kind}c]";

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
    my $text_dir = $self->get_text_subdir ();
    my $article_name = $self->get_article_name ();
    my %item_to_fragment_table = %{$self->get_item_to_fragment_table ()};
    my @dnos = $local_db->patterns_in_prel_of_kind ($kind);
    my @dnos_no_extension = map { strip_extension ($_) } @dnos;
    my @sorted_dnos
	= sort { $self->fragment_less_than ($a, $b) } @dnos_no_extension;

    foreach my $i (1 .. scalar @sorted_dnos) {
	my $fragment = $sorted_dnos[$i - 1];
	my $fragment_number = fragment_number ($fragment);
	my $item = "${article_name}:${kind}pattern:${i}";
	$item_to_fragment_table{$item}
	    = "${article_name}:fragment:${fragment_number}[${kind}p]";
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
	= sort { $self->fragment_less_than ($a, $b) } @definientia_no_extension;

    foreach my $i (1 .. scalar @sorted_definientia) {
	my $fragment = $sorted_definientia[$i - 1];
	my $fragment_number = fragment_number ($fragment);
	my $item = "${article_name}:${kind}definiens:${i}";
	$item_to_fragment_table{$item} = "${article_name}:fragment:${fragment_number}[${kind}f]";

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

    # DEBUG
    warn 'Fragment-to-item table:', "\n", Dumper (%fragment_to_item_table);

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
    my @sorted_theorems = sort { $self->fragment_less_than ($a, $b) } @theorems_no_extension;

    foreach my $i (1 .. scalar @sorted_theorems) {
	my $fragment_of_theorem = $sorted_theorems[$i - 1];
	my $fragment_number = fragment_number ($fragment_of_theorem);
	my $item = "${article_name}:deftheorem:${i}";
	$item_to_fragment_table{$item} = "${article_name}:fragment:${fragment_number}[dt]";
    }

    $self->_set_item_to_fragment_table (\%item_to_fragment_table);

    return 1;

}

sub load_schemes {
    my $self = shift;

    my $local_db = $self->get_local_database ();
    my %item_to_fragment_table = %{$self->get_item_to_fragment_table ()};
    my $article_name = $self->get_article_name ();

    my @schemes = $local_db->schemes_in_prel ();
    my @schemes_no_extension = map { strip_extension ($_) } @schemes;
    my @sorted_schemes = sort { $self->fragment_less_than ($a, $b) } @schemes_no_extension;

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
	= sort { $self->fragment_less_than ($a, $b) } @reductions_no_extension;

    foreach my $i (1 .. scalar @sorted_reductions) {
	my $fragment_of_reduction = $sorted_reductions[$i - 1];
	my $fragment_number = fragment_number ($fragment_of_reduction);
	my $item = "${article_name}:reduction:${i}";
	$item_to_fragment_table{$item} = "${article_name}:fragment:${fragment_number}";
    }

    $self->_set_item_to_fragment_table (\%item_to_fragment_table);

    return 1;

}

sub load_clusters_of_kind {
    my $self = shift;
    my $kind = shift;

    my $local_db = $self->get_local_database ();
    my $text_subdir = $self->get_text_subdir ();
    my %item_to_fragment_table = %{$self->get_item_to_fragment_table ()};
    my $article_name = $self->get_article_name ();

    my @clusters = $local_db->clusters_in_prel_of_kind ($kind);
    my @clusters_no_extension = map { strip_extension ($_) } @clusters;
    my @sorted_clusters
	= sort { $self->fragment_less_than ($a, $b) } @clusters_no_extension;

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
    my %fragment_of_lemma_table = %{$self->get_fragment_of_lemma_table ()};
    my $article_name = $self->get_article_name ();

    my @theorems = $local_db->theorems_in_prel ();
    my @theorems_no_extension = map { strip_extension ($_) } @theorems;
    my @sorted_theorems = sort { $self->fragment_less_than ($a, $b) } @theorems_no_extension;

    my $genuine_theorem_number = 1;
    foreach my $fragment_of_theorem (@sorted_theorems) {
	my $fragment_number = fragment_number ($fragment_of_theorem);

	if (defined $fragment_of_lemma_table{$fragment_number}) {
	    # Do nothing -- this isn't really a theorem from the
	    # original article, but a toplevel unexported lemma that
	    # got promoted to a theorem during our rewriting
	} else {

	    my $item = "${article_name}:theorem:${genuine_theorem_number}";
	    $item_to_fragment_table{$item}
		= "${article_name}:fragment:${fragment_number}";
	    $genuine_theorem_number++;
	}


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

    my @identifications = $local_db->identifications_in_prel_of_kind ($kind);
    my @identifications_no_extension = map { strip_extension ($_) } @identifications;
    my @sorted_identifications
	= sort { $self->fragment_less_than ($a, $b) } @identifications_no_extension;

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

    my $self = shift;

    my $local_db = $self->get_local_database ();
    my $local_db_location = $local_db->get_location ();
    my $article_name = $self->get_article_name ();
    my %item_to_fragment_table = %{$self->get_item_to_fragment_table ()};
    my %fragment_of_lemma_table = %{$self->get_fragment_of_lemma_table ()};

    my $article_itemized_wsx
	= "${local_db_location}/${article_name}.wsx.itemized";

    if (! ensure_readable_file ($article_itemized_wsx)) {
	croak ('Error: the split-and-itemized .wsx for ', $article_name, ' does not exist at the expected location (', $article_itemized_wsx, '.');
    }

    my $fragments_of_lemma_stylesheet
	= $self->path_for_stylesheet ('fragments-of-lemmas');

    my @fragments_of_lemmas = apply_stylesheet ($fragments_of_lemma_stylesheet,
						$article_itemized_wsx);
    chomp @fragments_of_lemmas;

    foreach my $fragment_of_lemma (@fragments_of_lemmas) {
	if ($fragment_of_lemma =~ /\A ([0-9]+) \N{SPACE} ([0-9]+) \z/) {
	    (my $lemma_number, my $fragment_number) = ($1, $2);
	    my $item = "${article_name}:lemma:${lemma_number}";
	    my $fragment = "${article_name}:fragment:${fragment_number}";
	    $item_to_fragment_table{$item} = $fragment;

	    # Record that this fragment number generates a lemma.  If this
	    # fragment shows up as a theorem, don't call it a theorem.
	    # The only theorems are the theorems in the original article.
	    $fragment_of_lemma_table{$fragment_number} = 0;

	} else {
	    croak ('Error: unable to make sense of the output line \'', $fragment_of_lemma, '\'.');
	}

    }

    $self->_set_item_to_fragment_table (\%item_to_fragment_table);
    $self->_set_fragment_of_lemma_table (\%fragment_of_lemma_table);

    return 1;

}

sub get_lemmas {
    my $self = shift;

    my @items = @{$self->get_all_items ()};

    my @lemmas = grep { / : lemma : / } @items;

    if (wantarray) {
	return @lemmas;
    } else {
	return join (' ', @lemmas);
    }

}

sub item_kind {
    my $item = shift;

    if ($item =~ /\A [a-z0-9_]+ : ([^:]+) : /) {
	return $1;
    } else {
	croak ('Error: unable to extract the kind from \'', $item, '\'.');
    }
}

sub resolve_local_item {
    my $self = shift;
    my $item = shift;

    my $article_name = $self->get_article_name ();
    my %fragment_to_item_table = %{$self->get_fragment_to_item_table ()};
    my %item_to_fragment_table = %{$self->get_item_to_fragment_table ()};

    if ($item =~ /\A ckb ([0-9]+) [:] ([a-z]+) [:] [0-9]+ ( [[] ([a-z]+) []] )? \z/) {
	(my $fragment_number, my $item_kind, my $tag) = ($1, $2, $4);
	my $fragment = "${article_name}:fragment:${fragment_number}" . (defined $tag ? '[' . $code_of_property{$tag} .']' : '');

	if (defined $tag) {
	    # nothing more to do
	} else {
	    if ($item_kind =~ /\A ([gkmruv]) constructor \z/) {
		$fragment .= "[${1}c]";
	    } elsif ($item_kind =~ /\A ([gjklmruv]) pattern \z/) {
		$fragment .= "[${1}p]";
	    } elsif ($item_kind eq 'deftheorem') {
		$fragment .= '[dt]';
	    } elsif ($item_kind =~ / \A ([kmrv]) definiens \z /) {
		$fragment .= "[${1}f]";
	    }
	}

	if (! defined $fragment_to_item_table{$fragment}) {
	    croak ('Error: what in the world is \'', $fragment, '\'? We cannot resolve \'', $item, '\'.  The fragment-to-item table looks like:', "\n", Dumper (%fragment_to_item_table), "\n", 'and the item-to-fragment table looks like', "\n", Dumper (%item_to_fragment_table));
	}
	my @generated_items = @{$fragment_to_item_table{$fragment}};

	# warn 'Generated items of ', $fragment, ' (coming from item ', $item, ') are:', "\n", Dumper (@generated_items);

	foreach my $generated_item (@generated_items) {
	    my $this_kind = item_kind ($generated_item);
	    if ($this_kind eq $item_kind) {
		if (defined $tag && $generated_item =~ / [[] $tag []] \z/) {
		    return $generated_item;
		} elsif (defined $tag) {
		    # nothing
		} elsif ($generated_item =~ / [[] [a-z]+ []] \z/) {
		    # nothing
		} else {
		    return $generated_item;
		}
	    }

	    # Lemma case
	    if ($generated_item =~ / : lemma : /) {
		return $generated_item;
	    }
	}

	# carp ('Warning: we failed to find an item among the items generated by ', $fragment, ' [', join (', ', @generated_items), '] congruent with the item ', $item, '.  Is this the case of a spurious constructor dependency coming from a redefinition?');

	croak 'Error: we could not resolve \'', $item, '\'.';

    } else {
	return $item;
    }

}

sub escape_item {
  my $item = shift;
  $item =~ s/\[/\\[/;
  $item =~ s/\]/\\]/;
  return $item;
}

sub dependencies {

    my $self = shift;

    my $local_db = $self->get_local_database ();
    my $location = $local_db->get_location ();
    my $text_dir = $local_db->get_text_subdir ();
    my $article_name = $self->get_article_name ();
    my %item_to_fragment_table = %{$self->get_item_to_fragment_table ()};
    my %fragment_to_item_table = %{$self->get_fragment_to_item_table ()};
    my @items = @{$self->get_all_items ()};

    # warn 'Item-to-fragment table is:', "\n", Dumper (%item_to_fragment_table);
    # warn 'Fragment-to-item table is:', "\n", Dumper (%fragment_to_item_table);

    my %dependencies = ();

    foreach my $item (@items) {
	if (! defined $item_to_fragment_table{$item}) {
	    croak ('Error: ', $item, ' is not present in the item-to-fragment table.');
	}

	my $fragment = $item_to_fragment_table{$item};

	# Pseudo fragments like 'ckb5[ch]' need to be changed: dump
	# their brackets '[' and ']' to recover the basename of the
	# .miz we are now after

	# warn 'In ItemizedArticle, getting dependencies of fragment ', $fragment, ', which comes from ', $item;

	if ($fragment =~ /\A ${article_name} [:] fragment [:] ([0-9]+) ([[] ([a-z]{2}) []])? /) {
	    (my $fragment_number, my $tag) = ($1, $3);
	    my $fragment_basename
		= defined $tag ? "ckb${fragment_number}${tag}"
		    : "ckb${fragment_number}";

	    my $fragment_miz = "${text_dir}/${fragment_basename}.miz";
	    my $fragment_abs_xml = "${text_dir}/${fragment_basename}.xml1";

	    if (-e $fragment_miz && -e $fragment_abs_xml) {

		my $fragment_doc = eval { $xml_parser->parse_file ($fragment_abs_xml) };

		if (! defined $fragment_doc) {
		    croak ('Error: the XML document at ', $fragment_abs_xml, '.');
		}

		my @fragment_deps = @{$local_db->dependencies_of ($fragment_basename)};

		my %deps = ();
		foreach my $dep (@fragment_deps) {
		    if ($dep =~ /\A ckb ${fragment_number} [:] /) {
			warn 'Throwing out a self-dependency...';
		    } else {
			my $resolved_dep = $self->resolve_local_item ($dep);
			if (defined $resolved_dep) {
			    $deps{$resolved_dep} = 0;
			}
		    }
		}

		my @deps_array = keys %deps;

		$dependencies{$item} = \@deps_array;

	    } else {
		# carp ('Warning: the fragment \'', $fragment, '\' does not exist in the local database.  Is this a case of a redefined constructor?');
	    }
	} else {
	    croak ('Error: unable to make sense of the fragment \'', $fragment, '\'.');
	}

    }

    return \%dependencies;

}

sub minimize {

    my $self = shift;
    my $parameters_ref = shift;

    my %parameters = defined $parameters_ref ? %{$parameters_ref} : ();

    my $local_db = $self->get_local_database ();

    my @all_articles = $local_db->articles ();

    my @proper_articles = grep { /\A ckb [0-9]+ \z/ } @all_articles;
    my @pseudo_articles = grep { /\A ckb [0-9]+ [a-z]{2} \z/ } @all_articles;

    # warn 'pseudo articles: ', "\n", Dumper (@pseudo_articles);

    my %proper_article_parameters = %parameters;
    $proper_article_parameters{'fast-theorems-and-schemes'} = 1;
    $local_db->minimize_articles (\@proper_articles, \%proper_article_parameters);

    # my %pseudo_article_parameters = %parameters;
    # $pseudo_article_parameters{'checker-only'} = 1;
    # $local_db->minimize_articles (\@pseudo_articles, \%pseudo_article_parameters);

    # We need to rewrite aids.  Sigh.
    # warn 'Rewriting aids, post minimization...';
    # $self->absolutize ();
    # $self->rewrite_pseudo_fragment_aids ();

    return 1;

}

sub absolutize {
    my $self = shift;
    my $local_db = $self->get_local_database ();
    return $local_db->absolutize ();
}

sub get_pseudo_fragments {
    my $self = shift;
    my $local_db = $self->get_local_database ();
    my @articles = $local_db->articles ();
    my @pseudo_fragments = ();
    foreach my $article (@articles) {
	if ($article =~ /\A ckb [0-9]+ [a-z]{2} \z/) {
	    push (@pseudo_fragments, $article);
	}
    }

    if (wantarray) {
	return @pseudo_fragments;
    } else {
	return \@pseudo_fragments;
    }

}

sub rewrite_pseudo_fragment_aids {

    my $self = shift;

    my $text_dir = $self->get_text_subdir ();

    my @pseudo_fragments = $self->get_pseudo_fragments ();
    my $rewrite_aid_stylesheet = $self->path_for_stylesheet ('rewrite-aid');

#    foreach my $extension ('xml1', 'eno1', 'dfs1', 'ecl1', 'eid1', 'epr1', 'erd1', 'esh1', 'eth1') {
    foreach my $extension ('xml1') {
	foreach my $pseudo_fragment (@pseudo_fragments) {
	    if ($pseudo_fragment =~ / ckb ([0-9]+) /) {
		my $proper_fragment = "ckb${1}";
		my $pseudo_fragment_xml = "${text_dir}/${pseudo_fragment}.${extension}";
		my $pseudo_fragment_xml_tmp = "${pseudo_fragment_xml}.tmp";
		if (-e $pseudo_fragment_xml) {
		    apply_stylesheet ($rewrite_aid_stylesheet,
				      $pseudo_fragment_xml,
				      $pseudo_fragment_xml_tmp,
				      { 'new' => $pseudo_fragment });
		    if (! -e $pseudo_fragment_xml_tmp) {
			croak ('wtf? ', $pseudo_fragment_xml_tmp, ' does not exist.');
		    }
		    File::Copy::move ($pseudo_fragment_xml_tmp,
				      $pseudo_fragment_xml)
			  or croak ('Error: unable to rename ', $pseudo_fragment_xml_tmp, ' to ', $pseudo_fragment_xml, ': ', $!);
		}
	    } else {
		croak ('Error: unable to make sense of the pseudo-fragment \'', $pseudo_fragment, '\'.');
	    }

	}
    }

    return 1;

}

1;
__END__
