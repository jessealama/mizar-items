package Article;

use Moose;
use Carp qw(croak carp);
use File::Basename qw(basename dirname);
use File::Copy qw(); # import nothing; we define our own 'copy' subroutine
use Regexp::DefaultFlags;
use XML::LibXML;
use POSIX qw(floor ceil);
use Data::Dumper;
use Readonly;
use charnames qw(:full);
use List::Util qw(shuffle);
use List::MoreUtils qw(first_index any);

# Our libraries
use FindBin qw($RealBin);
use lib "$RealBin/../lib";
use Utils qw(ensure_readable_file strip_extension extension delete_space write_string_to_file);
use Mizar;
use ItemizedArticle;
use LocalDatabase;
use Xsltproc qw(apply_stylesheet);

# The greatest number of fragments that an article may possess.  It
# should be at most 3 decimal digits.  This is because we may
# potentially add 2-letter suffixes to the names of our fragments.  We
# use 'ckb' as the prefix, followed by a number and maybe a two-letter
# constructor property/correctness condition code, thus, 'ckb50' or
# 'ckb50ab'.  If there are more than 999 fragments, then we could
# potentially use names like 'ckb1000ab', which is too long for a
# Mizar article name (these have to be at most 8 characters long).
Readonly my $MAX_FRAGMENTS => 999;

Readonly my $NUM_REQUIREMENTS => 32; # the number of requirements declared in builtin.pas

Readonly my %REQUIREMENTS_CONSTRUCTORS =>
    ( 1 => 'hidden:mconstructor:1',    # rqAny
      2 => 'hidden:rconstructor:1',    # rqEqualsTo
      3 => 'hidden:rconstructor:2',    # rqBelongsTo
      4 => 'xboole_0:vconstructor:1',  # rqEmpty
      5 => 'xboole_0:kconstructor:1',  # rqEmptySet
      6 => 'subset_1:mconstructor:1',  # rqElement
      7 => 'zfmisc_1:kconstructor:2',  # rqPowerSet
      8 => 'tarski:rconstructor:1',    # rqInclusion
      9 => 'subset_1:mconstructor:2',  # rqSubDomElem (?)
      10 => 'numbers:kconstructor:1',  # rqRealDom
      11 => 'numbers:kconstructor:5',  # rqNatDom
      12 => 'xcmplx_0:kconstructor:2', # rqRealAdd
      13 => 'xcmplx_0:kconstructor:3', # rqRealMult
      14 => 'arytm_0:rconstructor:1',  # rqLessOrEqual (?)
      15 => 'ordinal1:kconstructor:1', # rqSucc
      16 => 'xboole_0:kconstructor:2', # rqUnion
      17 => 'xboole_0:kconstructor:3', # rqIntersection
      18 => 'xboole_0:kconstructor:4', # rqSubtraction
      19 => 'xboole_0:kconstructor:5', # rqSymmetricDifference
      20 => 'xboole_0:rconstructor:1', # rqMeets
      21 => 'xcmplx_0:kconstructor:4', # rqRealNeg
      22 => 'xcmplx_0:kconstructor:5', # rqRealInv
      23 => 'xcmplx_0:kconstructor:6', # rqRealDiff
      24 => 'xcmplx_0:kconstructor:7', # rqRealDiv
      25 => 'xcmplx_0:vconstructor:1', # rqReal
      26 => 'xxreal_0:vconstructor:2', # rqPositive
      27 => 'xxreal_0:vconstructor:3', # rqNegative
      28 => 'ordinal1:vconstructor:7', # rqNatural
      29 => 'xcmplx_0:kconstructor:1', # rqImaginaryUnit
      30 => 'xcmplx_0:vconstructor:1', # rqComplex
      31 => 'ordinal1:kconstructor:4', # rqOmega
  );

Readonly my %REQUIREMENTS =>
    ( 0 => 'rqNone',
      1 => 'rqAny',
      2 => 'rqEqualsTo',
      3 => 'rqBelongsTo',
      4 => 'rqEmpty',
      5 => 'rqEmptySet',
      6 => 'rqElement',
      7 => 'rqPowerSet',
      8 => 'rqInclusion',
      9 => 'rqSubDomElem',
      10 => 'rqRealDom',
      11 => 'rqNatDom',
      12 => 'rqRealAdd',
      13 => 'rqRealMult',
      14 => 'rqLessOrEqual',
      15 => 'rqSucc',
      16 => 'rqUnion',
      17 => 'rqIntersection',
      18 => 'rqSubtraction',
      19 => 'rqSymmetricDifference',
      20 => 'rqMeets',
      21 => 'rqRealNeg',
      22 => 'rqRealInv',
      23 => 'rqRealDiff',
      24 => 'rqRealDiv',
      25 => 'rqReal',
      26 => 'rqPositive',
      27 => 'rqNegative',
      28 => 'rqNatural',
      29 => 'rqImaginaryUnit',
      30 => 'rqComplex',
      31 => 'rqOmega',
  );

# TODO map requirements to theorems

Readonly my %REQUIREMENTS_THEOREMS => (
    'rqNone' => [],
    'rqAny' => [],
    'rqEqualsTo' => [],
    'rqBelongsTo' => [],
    'rqEmpty' => [],
    'rqEmptySet' => [],
    'rqElement' => [],
    'rqPowerSet' => [],
    'rqInclusion' => [],
    'rqSubDomElem' => [],
    'rqRealDom' => [],
    'rqNatDom' => [],
    'rqRealAdd' => [],
    'rqRealMult' => [],
    'rqLessOrEqual' => [],
    'rqSucc' => [],
    'rqUnion' => [],
    'rqIntersection' => [],
    'rqSubtraction' => [],
    'rqSymmetricDifference' => [],
    'rqMeets' => [],
    'rqRealNeg' => [],
    'rqRealInv' => [],
    'rqRealDiff' => [],
    'rqRealDiv' => [],
    'rqReal' => [],
    'rqPositive' => [],
    'rqNegative' => [],
    'rqNatural' => [],
    'rqImaginaryUnit' => [],
    'rqComplex' => [],
    'rqOmega' => [],
);

Readonly my $SPACE => q{ };
Readonly my $LF => "\N{LF}";

has 'path' => (
    is => 'ro',
    isa => 'Str',
    reader => 'get_path',
    required => 1,
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

my $xml_parser = XML::LibXML->new (suppress_errors => 1,
				   suppress_warnings => 1);

sub BUILD {
    my $self = shift;

    my $path = $self->get_path ();
    if ( ! ensure_readable_file ($path)) {
	croak ('Error: the assigned path (', $path, ') is not a readable file.');
    }

    my $sheet_home = $self->get_stylesheet_home ();
    if (! -d $sheet_home) {
	croak ('Error: the supplied path (', $sheet_home, ') is not a directory.');
    }

    my $script_home = $self->get_script_home ();
    if (! -d $script_home) {
	croak ('Error: the supplied path (', $script_home, ') is not a directory.');
    }

    return $self;
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

sub path_for_stylesheet {
    my $self = shift;
    my $sheet = shift;
    my $stylesheet_home = $self->get_stylesheet_home ();
    return "${stylesheet_home}/${sheet}.xsl";
}

sub path_for_script {
    my $self = shift;
    my $script = shift;
    my $stylesheet_home = $self->get_script_home ();
    return "${stylesheet_home}/${script}";
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
    return $self->absolutize_extension ('xml');
}

sub absolutize_extension {
    my $self = shift;
    my $extension = shift;
    my $xml_path = $self->file_with_extension ($extension);
    my $abs_xml_path = $self->file_with_extension ("${extension}1");

    if (-e $xml_path && ! -e $abs_xml_path) {
	my $absrefs_stylesheet = $self->path_for_stylesheet ('addabsrefs');

	apply_stylesheet ($absrefs_stylesheet,
			  $xml_path,
			  $abs_xml_path);
    }

    return;
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

my %extension_to_element_table = ('eno' => 'Notations',
				  'erd' => 'ReductionRegistrations',
				  'epr' => 'PropertyRegistration',
				  'dfs' => 'Definientia',
				  'eid' => 'IdentifyRegistrations',
				  'ecl' => 'Registrations',
				  'esh' => 'Schemes',
				  'eth' => 'Theorems');

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

Readonly my %ITEM_KINDS_BY_EXTENSION => (
    'deftheorem' => 'xml1',
    'theorem' => 'xml1',
    'scheme' => 'xml1',
    'definiens' => 'dfs1',
    'cluster' => 'ecl1',
    'identification' => 'eid1',
    'reduction' => 'erd1',
    'pattern' => 'eno1',
    'property-registration' => 'epr1',
);

sub needed_non_constructors {
    my $self = shift;

    $self->absolutize ();
    $self->absolutize_environment ();

    my $abs_xml = $self->file_with_extension ('xml1');

    my $dependencies_stylesheet = $self->path_for_stylesheet ('dependencies');

    my @needed = ();

    foreach my $item_kind (keys %ITEM_KINDS_BY_EXTENSION) {
	my $extension = $ITEM_KINDS_BY_EXTENSION{$item_kind};
	my $xml_to_inspect = $self->file_with_extension ($extension);
	if (-e $xml_to_inspect) {
	    my @needed_for_extension
		= apply_stylesheet ($dependencies_stylesheet,
				    $xml_to_inspect,
				    undef,
				    { 'item-kind' => $item_kind }
				);
	    push (@needed, @needed_for_extension);
	}
    }

    @needed = map { lc ($_) } @needed;

    # There may be repetitions here
    my %items = ();
    foreach my $item (@needed) {
	$items{$item} = 0;
    }

    @needed = keys %items;

    # Throw out self-dependencies.  These can arise from definition items
    my $name = $self->name ();
    # warn 'Checking for self-dependencies: article is ', $name, ' and needed are:', Dumper (@needed);
    foreach my $item (@needed) {
	if ($item =~ /\A ${name} [:] /) {
	    delete $items{$item};
	}
    }

    @needed = keys %items;

    # # Grab theorems inferred from needed requirements

    # my @ere_lines = $self->ere_lines ();

    # foreach my $i (2 .. scalar @ere_lines - 1) {
    # 	#          ^    start from 2 because the first two requirements are bogus
    #     my $ere_line = $ere_lines[$i];
    # 	if ($ere_line !~ /\A [0] /) {
    # 	    my $requirement_name = $REQUIREMENTS{$i};

    # 	    if (! defined $requirement_name) {
    # 		croak ('Error: what is the symbolic name of requirement number ',$i, '?');
    # 	    }

    # 	    $items{$requirement_name} = 0;

    # 	}
    # }

    if (wantarray) {
	return @needed;
    } else {
	return join (' ', @needed);
    }
}

sub ere_lines {
    my $self = shift;

    my $ere = $self->file_with_extension ('ere');

    if (! ensure_readable_file ($ere)) {
	croak ('Error: the ere file for ', $self->name (), ' does not exist (or is unreadable).');
    }

    my @ere_lines = ();
    open (my $ere_fh, '<', $ere)
	or croak 'Unable to open an input filehandle for ', $ere, ': ', $!;
    while (defined (my $ere_line = <$ere_fh>)) {
	chomp $ere_line;
	push (@ere_lines, $ere_line);
    }
    close $ere_fh
	or croak 'Unable to close the output filehandle for ', $ere, ': ', $!;

    if (wantarray) {
	return @ere_lines;
    } else {
	return join (' ', @ere_lines);
    }
}

sub extract_constructor {
    my $constructor_node = shift;

    if ($constructor_node->exists ('@aid')
	    && $constructor_node->exists ('@kind')
		&& $constructor_node->exists ('@absnr')) {
	(my $aid) = $constructor_node->findvalue ('@aid');
	(my $kind) = $constructor_node->findvalue ('@kind');
	(my $absnr) = $constructor_node->findvalue ('@absnr');
	return "${aid}:${kind}constructor:${absnr}";
    } else {
	my $node_name = $constructor_node->nodeName ();
	my $node_owner = $constructor_node->ownerDocument ();
	my $uri = $node_owner->URI ();
	croak 'A(n) ', $node_name, ' node in ', $uri, ' lacks either an aid, kind or absnr attribute.';
    }
}

sub find_all_constructors {
    my $root_node = shift;

    my $constructor_xpath = 'descendant::Pred[@kind = "R"] | descendant::Func[@kind = "K"] | descendant::Adjective | descendant::Typ | descendant::Field';

    my @constructors = $root_node->findnodes ($constructor_xpath);

    @constructors = map { extract_constructor ($_) } @constructors;
    @constructors = map { lc ($_) } @constructors;

    # carp 'Extracted constructors:', "\n", Dumper (@constructors);

    my %constructor_table = ();

    foreach my $constructor (@constructors) {
	$constructor_table{$constructor} = 0;
    }

    @constructors = keys %constructor_table;

    if (wantarray) {
	return @constructors;
    } else {
	return join (' ', @constructors);
    }

}

sub needed_constructors {
    my $self = shift;

    $self->absolutize ();
    $self->absolutize_environment ();

    my $inferred_constructors_stylesheet
	= $self->path_for_stylesheet ('inferred-constructors');

    my %constructors;

    foreach my $extension (%ITEM_KINDS_BY_EXTENSION) {
	my $xml_to_inspect = $self->file_with_extension ($extension);
	if (-e $xml_to_inspect) {
	    my $doc = $xml_parser->parse_file ($xml_to_inspect);
	    my $root = $doc->documentElement ();
	    my @more_constructors = find_all_constructors ($root);
	    foreach my $constructor (@more_constructors) {
		$constructors{$constructor} = 0;
	    }
	}
    }

    # This doesn't work.  Let's content ourselves with emitting a
    # requirements item, rather than trying to guess what further
    # constructors the requirement item depends upon.

    # Look into the .ere file to see what further constructors might be needed
    # my $ere = $self->file_with_extension ('ere');
    # my @ere_lines = $self->ere_lines ();

    # foreach my $i (2 .. $NUM_REQUIREMENTS - 1) {
    # 	#          ^  the first two requirements are bogus
    # 	my $ere_line = $ere_lines[$i];

    # 	# warn 'ere line ', $i, ' is ', $ere_line;

    # 	if ($ere_line !~ /\A [0]/) {

    # 	    # Grab associated constructor
    # 	    my $required_constructor = $REQUIREMENTS_CONSTRUCTORS{$i};
    # 	    if (defined $required_constructor) {

    # 		# warn 'Grabbing ', $required_constructor, ' as a dependency from ', $ere;

    # 		$constructors{$required_constructor} = 0;
    # 	    } else {
    # 		croak ('Error: unable to determine the constructor(s) to which a non-zero entry (', $ere_line, ') at line ', $i, ' in ', $ere, ' corresponds.', "\n", 'Here are the lines of the ere file:', "\n", Dumper (@ere_lines));
    # 	    }
    # 	}
    # }

    # Throw out self-dependencies.  These can arise from definition items
    my $name = $self->name ();
    my @needed = keys %constructors;
    # warn 'Checking for self-dependencies among constructors: article is ', $name, ' and needed constructors are:', Dumper (@needed);
    foreach my $item (@needed) {
	if ($item =~ /\A ${name} [:] /) {
	    delete $constructors{$item};
	}
    }

    @needed = keys %constructors;

    if (wantarray) {
	return @needed;
    } else {
	return join (' ', @needed);
    }

}

sub properties_of_constructor {
    my $self = shift;
    my $constructor = shift;

    if ($constructor =~ /\A ([0-9a-z_]+) : (.) constructor : ([0-9]+) \z/) {
	my ($aid, my $kind, my $nr) = ($1, $2, $3);

	my $atr = $self->file_with_extension ('atr');

	my %params =
	    (
		'kind' => $kind,
		'aid' => $aid,
		'nr' => $nr,
	    );

	my $properties_stylesheet
	    = $self->path_for_stylesheet ('properties-of-constructor');
	my @props
	    = apply_stylesheet ($properties_stylesheet,
				$atr,
				undef,
				\%params);

	# carp ('Inside properties_of_constructor for ', $constructor, ' : ', join (' ', @props));

	if (wantarray) {
	    return @props;
	} else {
	    return join (' ', @props);
	}

    } else {
	croak ('Error (properties_of_constructor): Unable to make sense of the constructor \'', $constructor, '\'.');
    }



}

sub properties_of_needed_constructors {
    my $self = shift;

    my @needed_constructors = $self->needed_constructors ();

    # warn 'Needed constructors:', Dumper (@needed_constructors);

    my @needed = ();
    my $name = $self->name ();

    foreach my $constructor (@needed_constructors) {
	my @props = $self->properties_of_constructor ($constructor);
	# warn 'Needed properties of constructor ', $constructor, ' for ', $name, ':', "\n", (scalar @props == 0 ? '(none)' : Dumper (@props));
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
    my $parameters_ref = shift;
    my %parameters = defined $parameters_ref ? %{$parameters_ref} : ();

    return Mizar::accom ($self->get_path (),
		         \%parameters);
}

sub is_accomable {
    my $self = shift;
    return $self->accom ();
}

sub verify {
    my $self = shift;
    my $parameters_ref = shift;
    my %parameters = defined $parameters_ref ? %{$parameters_ref} : ();

    # Ugh
    delete $parameters{'fast-theorems-and-schemes'};
    delete $parameters{'randomize'};
    delete $parameters{'debug'};

    return Mizar::verifier ($self->get_path (),
			    \%parameters);
}

sub analyze {
    my $self = shift;
    my $parameters_ref = shift;
    my %parameters = defined $parameters_ref ? %{$parameters_ref} : ();

    $parameters{'analyzer'} = 0;

    # Ugh
    delete $parameters{'fast-theorems-and-schemes'};
    delete $parameters{'randomize'};
    delete $parameters{'debug'};

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
    my $parameters_ref = shift;
    my %parameters = defined $parameters_ref ? %{$parameters_ref} : ();
    return Mizar::wsmparser ($self->get_path (),
			     \%parameters);
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

    my $strip_property_stylesheet = $self->path_for_stylesheet ('strip-property');

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

		    carp ('We can dump property ', $property, ' of constructor ', $constructor, ' from ', $self->name ());

		    File::Copy::copy ($atr, $atr_orig)
			or croak ('Error: we were unable to update the .atr for ', $self->name (), ' to reflect its independence from the property ', $property, ' of constructor ', $constructor, '.', "\n");

		} else {

		    carp ('Cannot dump property ', $property, ' of constructor ', $constructor, ' from ', $self->name ());

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
    my $original_md5 = shift;
    my $parameters_ref = shift;

    my %parameters = defined $parameters_ref ? %{$parameters_ref} : ();
    my $article_xml = $self->file_with_extension ('xml');
    my $article_xml_saved = $self->file_with_extension ('xml.reference');

    if (defined $parameters{'debug'} && $parameters{'debug'}) {
	warn 'Minimizing ', $root_element_name, ' elements for ', $path, ' from ', $begin, ' to ', $end, '...';
    }

    if ($end < $begin) {

	return \%table;

    } elsif ($end == $begin) {

	# Try deleting
	delete $table{$begin};

	$self->write_element_table (\@elements,
				    \%table,
				    $path,
				    $root_element_name);

	my $analyzable = $self->analyze (\%parameters);

	if ($analyzable) {

	    if ($self->stripped_xml_has_md5sum ($original_md5)) {
		# nothing to do
	    } else {
		# Semantics have changed
		File::Copy::copy ($article_xml_saved, $article_xml);
		$table{$begin} = 0;
		$self->write_element_table (\@elements,
					    \%table,
					    $path,
					    $root_element_name);
	    }

	} else {

	    File::Copy::copy ($article_xml_saved, $article_xml);
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

	my $begin_analyzable = $self->analyze (\%parameters);

	if ($begin_analyzable) {
	    my $new_md5 = md5sum_for_file ($article_xml);
	    if ($self->stripped_xml_has_md5sum ($original_md5)) {
		# nothing to do
	    } else {
		# Semantics have changed
		File::Copy::copy ($article_xml_saved, $article_xml);
		$table{$begin} = 0;
		$self->write_element_table (\@elements,
					    \%table,
					    $path,
					    $root_element_name);
	    }
	} else {

	    File::Copy::copy ($article_xml_saved, $article_xml);
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

	my $end_analyzable = $self->analyze (\%parameters);

	if ($end_analyzable) {
	    my $new_md5 = md5sum_for_file ($article_xml);
	    if ($self->stripped_xml_has_md5sum ($original_md5)) {
		# nothing to do
	    } else {
		# Semantics have changed
		File::Copy::copy ($article_xml_saved, $article_xml);
		$table{$end} = 0;
		$self->write_element_table (\@elements,
					    \%table,
					    $path,
					    $root_element_name);
	    }
	} else {
	    File::Copy::copy ($article_xml_saved, $article_xml);
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
	my $lower_half_safe = $self->analyze (\%parameters);

	if ($lower_half_safe) {

	    my $new_md5 = md5sum_for_file ($article_xml);

	    if ($self->stripped_xml_has_md5sum ($original_md5)) {

		return ($self->minimize_elements (\@elements,
						  \%table,
						  $path,
						  $root_element_name,
						  $begin + $half_segment_length + 1,
						  $end,
						  $original_md5,
						  \%parameters));
	    } else {
		# Semantics have changed
		File::Copy::copy ($article_xml_saved, $article_xml);

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
						  $original_md5,
						  \%parameters)};
		return ($self->minimize_elements (\@elements,
						  \%table_for_lower_half,
						  $path,
						  $root_element_name,
						  $begin + $half_segment_length + 1,
						  $end,
						  $original_md5,
						  \%parameters));
	    }
	} else {

	    File::Copy::copy ($article_xml_saved, $article_xml);

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
					      $original_md5,
					      \%parameters)};
	    return ($self->minimize_elements (\@elements,
					      \%table_for_lower_half,
					      $path,
					      $root_element_name,
					      $begin + $half_segment_length + 1,
					      $end,
					      $original_md5,
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
    my $original_md5 = shift;
    my $parameters_ref = shift;

    my %parameters = defined $parameters_ref ? %{$parameters_ref} : ();

    my $article_xml = $self->file_with_extension ('xml');
    my $article_xml_saved = $self->file_with_extension ('xml.reference');

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

	my $analyzable = $self->analyze (\%parameters);

	if ($analyzable) {

	    my $new_md5 = md5sum_for_file ($article_xml);

	    if ($self->stripped_xml_has_md5sum ($original_md5)) {
		# nothing to do
	    } else {

		# Semantics have changed; restore the original XML
		File::Copy::copy ($article_xml_saved, $article_xml);

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

	} else {

	    File::Copy::copy ($article_xml_saved, $article_xml);

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

	my $begin_analyzable = $self->analyze (\%parameters);

	if ($begin_analyzable) {
	    my $new_md5 = md5sum_for_file ($article_xml);
	    if ($self->stripped_xml_has_md5sum ($original_md5)) {
		# nothing to do
	    } else {

		File::Copy::copy ($article_xml_saved, $article_xml);

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
	} else {

	    File::Copy::copy ($article_xml_saved, $article_xml);

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

	my $end_analyzable = $self->analyze (\%parameters);

	if ($end_analyzable) {

	    my $new_md5 = md5sum_for_file ($article_xml);

	    if ($self->stripped_xml_has_md5sum ($original_md5)) {
		# nothing to do
	    } else {

		# Semantics have changed
		File::Copy::copy ($article_xml_saved, $article_xml);

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

	} else {

	    File::Copy::copy ($article_xml_saved, $article_xml);

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
	my $lower_half_safe = $self->analyze (\%parameters);

	if ($lower_half_safe) {

	    my $new_md5 = md5sum_for_file ($article_xml);

	    if ($self->stripped_xml_has_md5sum ($original_md5)) {

		return ($self->minimize_by_article (\@elements,
						    \@articles,
						    \%table,
						    $path,
						    $root_element_name,
						    $begin + $half_segment_length + 1,
						    $end,
						    $original_md5,
						    \%parameters));
	    } else {

		File::Copy::copy ($article_xml_saved, $article_xml);

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
						    $original_md5,
						    \%parameters)};
		return ($self->minimize_by_article (\@elements,
						    \@articles,
						    \%table_for_lower_half,
						    $path,
						    $root_element_name,
						    $begin + $half_segment_length + 1,
						    $end,
						    $original_md5,
						    \%parameters));


	    }

	} else {

	    File::Copy::copy ($article_xml_saved, $article_xml);

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
						$original_md5,
						\%parameters)};
	    return ($self->minimize_by_article (\@elements,
						\@articles,
						\%table_for_lower_half,
						$path,
						$root_element_name,
						$begin + $half_segment_length + 1,
						$end,
						$original_md5,
						\%parameters));
	}
    }
}

sub aid_for_element {
  my $element = shift;
  $element->exists ('@aid') ? return $element->findvalue ('@aid') : return undef;
}

sub md5sum_for_file {
    my $path = shift;
    my $md5sum = `md5sum $path | cut -f 1 -d ' '`;
    chomp $md5sum;
    return $md5sum;
}

sub stripped_xml_has_md5sum {
    my $self = shift;
    my $other_md5sum = shift;

    my $article_xml = $self->file_with_extension ('xml');
    my $article_xml_stripped = $self->file_with_extension ('xml.temp.stripped');

    my $uninteresting_attributes_stylesheet
	= $self->path_for_stylesheet ('uninteresting-attributes');

    apply_stylesheet ($uninteresting_attributes_stylesheet,
		      $article_xml,
		      $article_xml_stripped,
		      undef);

    my $stripped_md5sum = md5sum_for_file ($article_xml_stripped);

    return ($stripped_md5sum eq $other_md5sum ? 1 : 0);

}

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
    my $article_xml = $self->file_with_extension ('xml');
    my $article_xml_saved = $self->file_with_extension ('xml.reference');
    File::Copy::copy ($article_xml, $article_xml_saved);

    # Strip 'uninteresting' attributes so that we can later compare
    # quickly whether the semantics of the article changes if we mess
    # around with the environment
    my $article_xml_stripped = $self->file_with_extension ('xml.stripped');

    my $uninteresting_attributes_stylesheet
	= $self->path_for_stylesheet ('uninteresting-attributes');

    apply_stylesheet ($uninteresting_attributes_stylesheet,
		      $article_xml,
		      $article_xml_stripped,
		      undef);

    my $article_xml_md5 = md5sum_for_file ($article_xml_stripped);

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
					    $article_xml_md5,
					    \%parameters)};

	if (defined $parameters{'randomize'} && $parameters{'randomize'}) {

	    # Randomize the order of the elements
	    my %minimized_by_article_table_shuffled = ();
	    my %shuffled_index = ();
	    my @elements_shuffled = shuffle @elements;
	    foreach my $i (0 .. scalar @elements - 1) {
		my $element = $elements[$i];
		my $shuffled_idx = first_index { $_ == $element } @elements_shuffled;
		if ($shuffled_idx < 0) {
		    croak 'We failed to find element', $SPACE, $i, $SPACE, 'in the shuffled list.';
		}
		$shuffled_index{$i} = $shuffled_idx;
	    }

	    foreach my $index (keys %minimized_by_article_table) {
		my $shuffled_index = $shuffled_index{$index};
		$minimized_by_article_table_shuffled{$shuffled_index} = 0;
	    }

	    @elements = @elements_shuffled;
	    %minimized_by_article_table = %minimized_by_article_table_shuffled;

	    carp 'Elements are shuffled.';

	}

	apply_stylesheet ($uninteresting_attributes_stylesheet,
			  $article_xml,
			  $article_xml_stripped,
			  undef);

	my $md5_now = md5sum_for_file ($article_xml_stripped);
	if ($md5_now ne $article_xml_md5) {
	    croak 'Error: the MD5 sum for ', $self->name (), ' changed between whole-article minimization.';
	}


	my %minimized_table
	    = %{$self->minimize_elements (\@elements,
					  \%minimized_by_article_table,
					  $article_with_extension, $root_element_name,
					  0,
					  scalar @elements - 1,
					  $article_xml_md5,
					  \%parameters)};

	# # Restore
	# $self->verify (\%parameters);

	print {*STDERR} $self->name (), '.', $extension_to_minimize, ' : ';
	print {*STDERR} '[';
	foreach my $i (0 .. scalar @elements - 1) {
	    if (defined $minimized_table{$i}) {
		print {*STDERR} '+';
	    } else {
		print {*STDERR} '-';
	    }
	}
	print {*STDERR} ']', "\n";

	return keys %minimized_table;

    } else {
	return ();
    }
}

sub element_to_item {
    my $element = shift;
    my $name = $element->nodeName ();
    my $name_lc = lc $name;
    if (! $element->exists ('@aid')) {
	die 'Error: unable to handle an element that lacks an aid attribute.';
    }
    my $aid = $element->findvalue ('@aid');
    my $aid_lc = lc $aid;
    if ($name_lc =~ /\A ([cfr])cluster \z/) {
	my $cluster_kind = $1;
	if (! $element->exists ('@nr')) {
	    die 'Error: unable to handle a cluster element that lacks an nr attribute.';
	}
	my $nr = $element->findvalue ('@nr');
	return "${aid_lc}:${cluster_kind}cluster:${nr}";
    } else {
	die 'Error: unable to handle an element whose name is \'', $name, '\'.';
    }
}

sub prefer_suggested {
    my $element_1 = shift;
    my $element_2 = shift;
    my $suggestions_ref = shift;

    my @suggestions = defined $suggestions_ref ? @{$suggestions_ref} : ();

    my $element_1_name = element_to_item ($element_1);
    my $element_2_name = element_to_item ($element_2);

    my $element_1_idx = first_index { $_ eq $element_1_name } @suggestions;
    my $element_2_idx = first_index { $_ eq $element_2_name } @suggestions;

    if ($element_1_idx < 0) {
	if ($element_2_idx < 0) {
	    return 0;
	} else {
	    return 1;
	}
    } elsif ($element_2_idx < 0) {
	return -1;
    } else {
	return $element_1_idx <=> $element_2_idx;
    }

}

sub shortest_initial_verifiable_subsequence {
    my $self = shift;
    my $elements_ref = shift;
    my $path = shift;
    my $root_element_name = shift;
    my $parameters_ref = shift;

    my @elements = defined $elements_ref ? @{$elements_ref} : ();
    my %parameters = defined $parameters_ref ? %{$parameters_ref} : ();

    # carp ('Computing shortest initial segment of ', scalar @elements, ' ', $root_element_name, ' elements (saving results under ', $path, '.)');

    my $article_name = $self->name ();
    my $article_path = $self->get_path ();

    my %needed = ();
    my $lo = 0;
    my $hi = scalar @elements - 1;

    # Initially everything is needed
    foreach my $i (0 .. scalar @elements - 1) {
	$needed{$i} = 0;
    }

    # Write this to disk
    $self->write_element_table (\@elements,
				\%needed,
				$path,
				$root_element_name);

    while ($lo + 1 < $hi) {

	my $guess = $lo + floor (($hi - $lo) / 2);

	# carp ('(lo,hi,guess) = (', $lo, ',', $hi, ',', $guess, ')');

	# Delete everything after the index
	foreach my $i ($guess + 1 .. scalar @elements - 1) {
	    delete $needed{$i};
	}

	# Write this to disk
	$self->write_element_table (\@elements,
				    \%needed,
				    $path,
				    $root_element_name);

	# Try verifying
	my $verifiable = $self->verify (\%parameters);

	if ($verifiable) {
	    $hi = $guess;
	} else {

	    # Restore
	    foreach my $i ($guess + 1 .. scalar @elements - 1) {
		$needed{$i} = 0;
	    }

	    # Write this to disk
	    $self->write_element_table (\@elements,
					\%needed,
					$path,
					$root_element_name);

	    $lo = $guess;
	}

    }

    return $hi;

}

sub minimize_linear_scan {
    my $self = shift;
    my $elements_ref = shift;
    my $path = shift;
    my $root_element_name = shift;
    my $begin = shift;
    my $end = shift;
    my $parameters_ref = shift;

    my @elements = defined $elements_ref ? @{$elements_ref} : ();
    my %parameters = defined $parameters_ref ? %{$parameters_ref} : ();

    my %needed = ();

    # Initially everything is needed
    foreach my $i ($begin .. $end) {
	$needed{$i} = 0;
    }

    # Write this to disk
    $self->write_element_table (\@elements,
				\%needed,
				$path,
				$root_element_name);

    # Initial check
    my $initially_verifiable = $self->verify (\%parameters);

    if ($initially_verifiable != 1) {
	croak ('Error: before doing a linear scan minimization of ', $self->name (), ', the article is not verifiable.');
    }

    carp ('Linear scan minimization of ', scalar @elements, ' from index ', $begin, ' to ', $end);

    foreach my $i ($begin .. $end) {
	delete $needed{$i};
	$self->write_element_table (\@elements,
				    \%needed,
				    $path,
				    $root_element_name);
	my $deletable = $self->verify (\%parameters);
	if ($deletable == 0) {
	    $needed{$i} = 0;
	    $self->write_element_table (\@elements,
					\%needed,
					$path,
					$root_element_name);
	    carp ('Element ', $i, ' is not deletable.');
	} else {
	    carp ('Element ', $i, ' is deletable.');
	}
    }

    # Restore
    $self->verify (\%parameters);

    my @needed = keys %needed;
    carp ('Of the elements from ', $begin, ' to ', $end, ', linear scan shows that these are needed:', "\n", Dumper (@needed));

    return;

}

sub minimize_extension_with_suggestion {
    my $self = shift;
    my $extension_to_minimize = shift;
    my $suggestions_ref = shift;
    my $parameters_ref = shift;

    my @suggestions = defined $suggestions_ref ? @{$suggestions_ref} : ();
    my %parameters = defined $parameters_ref ? %{$parameters_ref} : ();

    # carp ('Minimizing the ', $extension_to_minimize, ' file using the suggestions ', "\n", scalar @suggestions == 0 ? '(empty list)' : Dumper (@suggestions));

    my $root_element_name = $extension_to_element_table{$extension_to_minimize};
    my $article_name = $self->name ();

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

	if (scalar @suggestions == 0) {

	    # Try verifying with an empty set of suggestions
	    foreach my $i (0 .. scalar @elements - 1) {
		delete $initial_table{$i};
	    }

	    # Write this to disk
	    $self->write_element_table (\@elements,
					\%initial_table,
					$article_with_extension,
					$root_element_name);

	    # Try verifying
	    my $all_deletable = $self->verify (\%parameters);

	    if ($all_deletable) {
		# carp ('Warning: from ', $article_name, ' we can delete all ', $root_element_name, ' elements, as suggested.');
		return;
	    } else {
		# carp ('Warning: from ', $article_name, ' we cannot delete all ', $root_element_name, ' elements, as suggested.', "\n", 'Proceeding using normal brute-force minimization...');

		# Try verifying with an empty set of suggestions
		foreach my $i (0 .. scalar @elements - 1) {
		    $initial_table{$i} = 0;
		}

		# Write this to disk
		$self->write_element_table (\@elements,
					    \%initial_table,
					    $article_with_extension,
					    $root_element_name);

		# Restore
		$self->verify (\%parameters);

		return $self->minimize_extension ($extension_to_minimize, \%parameters);

	    }

	} else {

	    my @sorted_elements =
		sort { prefer_suggested ($a, $b, \@suggestions) } @elements;

	    # my @sorted_element_names = map { element_to_item ($_) } @sorted_elements;
	    # carp ('Sorted environment, adhering to suggestion:', "\n", Dumper (@sorted_element_names));

	    # Initially, everything is needed.
	    foreach my $i (0 .. scalar @sorted_elements - 1) {
		$initial_table{$i} = 0;
	    }

	    $self->write_element_table (\@sorted_elements,
					\%initial_table,
					$article_with_extension,
					$root_element_name);

	    # First do binary search for the smallest initial segment
	    # of @elements that is verifiable.
	    my $index = $self->shortest_initial_verifiable_subsequence (\@sorted_elements,
									$article_with_extension,
									$root_element_name,
								        \%parameters);

	    if ($index < 0) {

		# carp ('Warning: the article is verifiable using an empty ', $root_element_name, ' environment!');

		return;

	    } else {

		# carp ('Warning: index of shortest initial verifiable subsequence: ', $index);
		# carp ('Deleting all elements from ', $index + 1, ' to ', scalar @sorted_elements - 1);

		foreach my $i ($index + 1 .. scalar @sorted_elements - 1) {
		    delete $initial_table{$i};
		}

		$self->write_element_table (\@sorted_elements,
					    \%initial_table,
					    $article_with_extension,
					    $root_element_name);

		# Check
		my $before_linear_scan_ok = $self->verify (\%parameters);

		if ($before_linear_scan_ok != 1) {
		    croak ('Error: before doing a brute force binary search, we find that ', $self->name (), ' is not verifiable.');
		}

		my %really_needed_table = %{$self->minimize_elements (\@sorted_elements,
								      \%initial_table,
								      $article_with_extension,
								      $root_element_name,
								      0,
								      $index,
								      \%parameters)};

		my @really_needed = keys %really_needed_table;

		# carp ('Indices of elements that are really needed: ', Dumper (@really_needed));

		return keys %really_needed_table;

	    }

	    return keys %initial_table;

	}

    } else {
	return;
    }
}

sub minimize_environment_with_suggestions {

    my $self = shift;
    my $suggestions_ref = shift;
    my $parameters_ref = shift;

    my %suggestions = defined $suggestions_ref ? %{$suggestions_ref} : ();
    my %parameters = defined $parameters_ref ? %{$parameters_ref} : ();

    my @extensions_to_minimize = ('eno', 'erd', 'epr', 'dfs', 'eid', 'ecl');

    if (defined $parameters{'fast-theorems-and-schemes'}) {
	$self->prune_theorems_and_schemes ();
	delete $parameters{'fast-theorems-and-schemes'};
    } else {
	push (@extensions_to_minimize, 'esh', 'eth');
    }

    foreach my $extension (@extensions_to_minimize) {
	if (defined $suggestions{$extension}) {
	    my @suggestions = @{$suggestions{$extension}};
	    $self->minimize_extension_with_suggestion ($extension, \@suggestions, \%parameters);
	} else {
	    $self->minimize_extension ($extension, \%parameters);
	}
    }

}

sub minimize_environment {

    my $self = shift;
    my $parameters_ref = shift;

    my %parameters = defined $parameters_ref ? %{$parameters_ref} : ();

    my @extensions_to_minimize = ('eno', 'erd', 'epr', 'dfs', 'eid', 'ecl');

    if (defined $parameters{'fast-theorems-and-schemes'}) {
	$self->prune_theorems_and_schemes ();
	delete $parameters{'fast-theorems-and-schemes'};
    } else {
	push (@extensions_to_minimize, 'esh', 'eth');
    }

    foreach my $extension (@extensions_to_minimize) {
	$self->minimize_extension ($extension, \%parameters);
    }

}

sub write_ere_table {

    # warn 'writing new ere table';

    my $self = shift;
    my %table = %{shift ()};
    my %original_requirements = %{shift ()};

    my $ere_path = $self->file_with_extension ('ere');

    open (my $ere_fh, '>', $ere_path)
	or croak ('Error: unable to open an output filehandle for ', $ere_path, '.');
    foreach my $i (0 .. $NUM_REQUIREMENTS - 1) {
	if (defined $table{$i}) {
	    if (defined $original_requirements{$i}) {
		print {$ere_fh} $original_requirements{$i};
	    } else {
		close $ere_fh;
		croak ('Error: we are supposed to emit requirement #', $i, ', but we do not know the original value in for this requirement.');
	    }
	} else {
	    print {$ere_fh} '0';
	}

	print {$ere_fh} "\n";
    }

    close $ere_fh
	or croak ('Error: unable to close the output filehandle for ', $ere_path, '.');

    # warn 'ere file now looks like:', "\n", `cat $ere_path`;

    return 1;

}

sub minimize_by_requirement {

    # warn 'we are minimizing requirements';

    my $self = shift;
    my %table = %{shift ()};
    my %original_requirements = %{shift ()};
    my $begin = shift;
    my $end = shift;
    my $parameters_ref = shift;

    my %parameters = defined $parameters_ref ? %{$parameters_ref} : ();

    my $err_path = $self->file_with_extension ('err');

    if ($end < $begin) {

	return \%table;

    } elsif ($end == $begin) {

	delete $table{$end};

	# Save the table to disk
	$self->write_ere_table (\%table, \%original_requirements);

	my $deletable = $self->verify (\%parameters);

	if (! $deletable) {

	    # DEBUG
	    # warn 'We cannot dump requirement #', $end, '; errors:', "\n", `cat $err_path`;

	    $table{$end} = 0;
	    $self->write_ere_table (\%table, \%original_requirements);
	} else {
	    # warn 'We can dump requirement #', $begin;
	}

	return \%table;

    } elsif ($begin + 1 == $end) {

	delete $table{$begin};
	$self->write_ere_table (\%table, \%original_requirements);

	my $begin_deletable = $self->verify (\%parameters);

	if (! $begin_deletable) {

	    # warn 'We cannot dump requirement #', $begin, '; errors:', "\n", `cat $err_path`;

	    $table{$begin} = 0;
	    $self->write_ere_table (\%table, \%original_requirements);
	} else {
	    # warn 'We can dump requirement #', $begin;
	}

	delete $table{$end};

	$self->write_ere_table (\%table, \%original_requirements);

	my $end_deletable = $self->verify (\%parameters);

	if (! $end_deletable) {

	    # warn 'We cannot dump requirement #', $begin, '; errors:', "\n", `cat $err_path`;

	    $table{$begin} = 0;
	    $self->write_ere_table (\%table, \%original_requirements);
	} else {
	    # warn 'We can dump requirement #', $end;
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
	$self->write_ere_table (\%table, \%original_requirements);

	# Check that deleting the lower half is safe
	my $lower_half_safe = $self->verify (\%parameters);

	if ($lower_half_safe) {

	    foreach my $i ($begin .. $begin + $half_segment_length) {
		# warn 'We can dump requirement #', $i;
	    }

	    return ($self->minimize_by_requirement (\%table,
						    \%original_requirements,
						    $begin + $half_segment_length + 1,
						    $end,
						    \%parameters));

	} else {

	    # warn 'We cannot dump all requirements between ', $begin, ' and ', $begin + $half_segment_length, '; errors:', "\n", `cat $err_path`;

	    # Restore the lower half
	    foreach my $i ($begin .. $begin + $half_segment_length) {
		$table{$i} = 0;
	    }

	    $self->write_ere_table (\%table, \%original_requirements);

	    # Minimize just the lower half
	    my %table_for_lower_half
		= %{$self->minimize_by_requirement (\%table,
						    \%original_requirements,
						    $begin,
						    $begin + $half_segment_length,
						    \%parameters)};
	    return ($self->minimize_by_requirement (\%table_for_lower_half,
						    \%original_requirements,
						    $begin + $half_segment_length + 1,
						    $end,
						    \%parameters));
	}
    }
}

sub minimize_requirements {
    my $self = shift;
    my $parameters_ref = shift;

    my %parameters = defined $parameters_ref ? %{$parameters_ref} : ();

    my $ere = $self->file_with_extension ('ere');

    if (! ensure_readable_file ($ere)) {
	croak ('Error: the needed .ere file does not exist (or is unreadable).');
    }

    my @ere_lines = $self->ere_lines ();

    # initially everything is needed
    my %needed_requirements = ();
    foreach my $i (0 .. $NUM_REQUIREMENTS - 1) {
	$needed_requirements{$i} = 0;
    }

    my %original_requirements = ();
    foreach my $i (0 .. $NUM_REQUIREMENTS - 1) {
	$original_requirements{$i} = $ere_lines[$i];
    }

    my %minimal_needed
	= %{$self->minimize_by_requirement (\%needed_requirements,
					    \%original_requirements,
					    2, # don't dump the first
                                               # two requirements
					    $NUM_REQUIREMENTS - 1,
					    \%parameters)};

    # foreach my $needed_index (keys %minimal_needed) {
    # 	warn 'We really need requirement #', $needed_index;
    # }

    # warn 'After minimizing requirements, the ere file looks like:', "\n", `cat $ere`;

    return 1;

}

sub minimize {
    my $self = shift;
    my $parameters_ref = shift;

    my %parameters = defined $parameters_ref ? %{$parameters_ref} : ();

    $self->minimize_environment (\%parameters);
    $self->minimize_properties (\%parameters);
    $self->minimize_requirements (\%parameters);

    return 1;
}

sub minimize_with_suggested_environment {
    my $self = shift;
    my $suggested_environment_ref = shift;
    my $parameters_ref = shift;

    my %suggested_environment
	= defined $suggested_environment_ref ? %{$suggested_environment_ref}
	                                     : ();
    my %parameters
	= defined $parameters_ref ? %{$parameters_ref}
	                          : ();


    $self->minimize_environment_with_suggestions (\%suggested_environment,
						  \%parameters);
    $self->minimize_properties (\%parameters);
    $self->minimize_requirements (\%parameters);

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

    my $path = $self->get_path ();

    # warn 'Property dependencies for :', $path, "\n", Dumper (@property_dependencies);

    push (@needed,
	  @non_constructor_dependencies,
	  @constructor_dependencies,
	  @property_dependencies);

    if (wantarray) {
	return @needed;
    } else {
	return join (' ', @needed);
    }

}

sub list_as_token_string {
  my @lst = @{shift ()};
  return ',' . join (',', @lst) . ',';
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
    my $parameters_ref = shift;

    my %parameters = defined $parameters_ref ? %{$parameters_ref} : ();

    my $article_basename = $self->name ();
    my $stylesheet_home = $self->get_stylesheet_home ();
    my $script_home = $self->get_script_home ();
    my $local_db = LocalDatabase->new (location => $target_directory,
				       stylesheet_home => $stylesheet_home,
				       script_home => $script_home);

    my $target_text_subdir = $local_db->get_text_subdir ();
    my $target_prel_subdir = $local_db->get_prel_subdir ();

    # Copy the article miz to the new subdirectory

    my $article_in_target_dir = $self->copy (File::Spec->catdir ($target_directory));

    my $article_evl_in_target_dir = $article_in_target_dir->file_with_extension ('evl');

    # Transform the new miz
    print 'Rewriting the text of ', $article_basename, ': ';

    my $expand_canceled_stylesheet = $self->path_for_stylesheet ('expand-canceled');
    my $split_stylesheet = $self->path_for_stylesheet ('split');
    my $pp_stylesheet = $self->path_for_stylesheet ('wsm');
    my $itemize_stylesheet = $self->path_for_stylesheet ('itemize');
    my $wsm_stylesheet = $self->path_for_stylesheet ('wsm');
    my $extend_evl_stylesheet = $self->path_for_stylesheet ('extend-evl');
    my $conditions_and_properties_stylesheet = $self->path_for_stylesheet ('conditions-and-properties');
    my $trim_properties_and_conditions_stylesheet = $self->path_for_stylesheet ('trim-properties-and-conditions');
    my $factor_proofs_stylesheet = $self->path_for_stylesheet ('factor-proofs');
    my $wrm_stylesheet = $self->path_for_stylesheet ('wrm');

    $article_in_target_dir->without_reservations ()
	or confess ('Error computing the without-reservations form of ', $article_basename, ': ', $!);

    print 'done.', "\n";

    my $article_wsx_in_target_dir
	= $article_in_target_dir->file_with_extension ('wsx');
    my $article_tpr_in_target_dir
	= $article_in_target_dir->file_with_extension ('tpr');
    my $article_itemized_wsx_in_target_dir
	= $article_in_target_dir->file_with_extension ('wsx.itemized');

    print 'Expand canceled theorems: ', $article_basename, ': ';
    apply_stylesheet ($expand_canceled_stylesheet,
		      $article_wsx_in_target_dir,
		      $article_wsx_in_target_dir)
	or
	    croak ('Error: xsltproc did not exit cleanly when applying the expand-canceled stylesheet at ', $expand_canceled_stylesheet, ' to ', $article_wsx_in_target_dir, '.', "\n");

    print 'done.', "\n";

    print 'Split: ';
    apply_stylesheet ($split_stylesheet,
		      $article_wsx_in_target_dir,
		      $article_wsx_in_target_dir)
	or
	    croak ('Error: xsltproc did not exit cleanly when applying the split stylesheet at ', $split_stylesheet, ' to ', $article_wsx_in_target_dir, '.', "\n");

    print 'done.', "\n";

    print 'Factor proofs: ';
    apply_stylesheet ($factor_proofs_stylesheet,
		      $article_wsx_in_target_dir,
		      $article_wsx_in_target_dir);

    $article_in_target_dir->accom ();
    $article_in_target_dir->msplit ();

    apply_stylesheet ($pp_stylesheet,
		      $article_wsx_in_target_dir,
		      $article_tpr_in_target_dir,
		      { 'suppress-environment' => 1 });

    $article_in_target_dir->mglue ();
    $article_in_target_dir->accom ();
    $article_in_target_dir->msmify ();
    $article_in_target_dir->wsmparser ();

    print 'Split again', $article_basename, ': ';
    apply_stylesheet ($split_stylesheet,
		      $article_wsx_in_target_dir,
		      $article_wsx_in_target_dir)
	or
	    croak ('Error: xsltproc did not exit cleanly when applying the split stylesheet at ', $split_stylesheet, ' to ', $article_wsx_in_target_dir, '.', "\n");

    print 'done.', "\n";

    if (! $article_in_target_dir->is_verifiable ()) {
	confess 'The transformed article is not verifiable.';
    }

    print 'Itemize ', $article_basename, ': ';
    apply_stylesheet ($itemize_stylesheet,
		      $article_wsx_in_target_dir,
		      $article_itemized_wsx_in_target_dir)
	or croak ('Error: xsltproc did not exit cleanly when applying the itemize stylesheet at ', $itemize_stylesheet, ' to ', $article_wsx_in_target_dir, '.', "\n");

    print 'done.', "\n";

    # Now write the fragments

    my $itemized_article_doc = eval { $xml_parser->parse_file ($article_itemized_wsx_in_target_dir) };
    if (! defined $itemized_article_doc) {
	croak ('Error: the XML in ', $article_itemized_wsx_in_target_dir, ' is not well-formed.', "\n");
    }

    my @fragments = $itemized_article_doc->findnodes ('/Fragments/Text-Proper');

    if (scalar @fragments > $MAX_FRAGMENTS) {
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

	if ($local_db->verify ($fragment_basename, \%parameters)) {
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

    print 'Absolutizing the fragment XMLs...';

    $local_db->absolutize ();

    print 'done.', "\n";

    my $itemized_article = ItemizedArticle->new (local_database => $local_db,
					         stylesheet_home => $stylesheet_home,
					         script_home => $script_home);

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

    if (! -d $new_path_dir) {
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
    my $stylesheet_home = $self->get_stylesheet_home ();
    my $script_home = $self->get_script_home ();
    my $new_article = Article->new (path => $new_path_miz,
				    stylesheet_home => $stylesheet_home,
				    script_home => $script_home);

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
    my $stylesheet_home = $self->get_stylesheet_home ();
    my $script_home = $self->get_script_home ();

    my $new_article = Article->new (path => $new_path_miz,
				    stylesheet_home => $stylesheet_home,
				    script_home => $script_home);

    return $new_article;

}

sub msmify {
    my $self = shift;

    $self->accom ()
	or croak ('Error: unable to accom ', $self->name (), '.');
    $self->wsmparser ()
	or croak ('Error: wsmparser failed on ', $self->name (), '.');
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

sub text_node_without_reservations {
    my $text_node = shift;
    return $text_node; # no transformation
}

sub is_reservation_node {
    my $node = shift;
    my $node_name = $node->nodeName ();
    if ($node_name eq 'Item') {
	if ($node->hasAttribute ('kind')) {
	    my $kind = $node->getAttribute ('kind');
	    return ($kind eq 'Reservation');
	} else {
	    return 0;
	}
    } else {
	return 0;
    }
}

sub is_implicitly_qualified_segment {
    my $node = shift;
    my $node_type = $node->nodeType ();
    if ($node_type == XML::LibXML::XML_ELEMENT_NODE) {
	my $node_name = $node->nodeName ();
	return ($node_name eq 'Implicitly-Qualified-Segment');
    } else {
	return 0;
    }
}

sub is_fraenkel_term {
    my $node = shift;
    my $node_type = $node->nodeType ();
    if ($node_type == XML::LibXML::XML_ELEMENT_NODE) {
	my $node_name = $node->nodeName ();
	return ($node_name eq 'Fraenkel-Term');
    } else {
	return 0;
    }
}

sub new_element {
    my $name = shift;
    my $owner = shift;
    my $new_node = XML::LibXML::Element->new ($name);
    $new_node->setOwnerDocument ($owner);
    return $new_node;
}

sub fraenkel_binds_variable {
    my $fraenkel = shift;
    my $variable = shift;

    my $fraenkel_line = $fraenkel->hasAttribute ('line') ? $fraenkel->getAttribute ('line') : '(undefined)';
    my $fraenkel_col = $fraenkel->hasAttribute ('col') ? $fraenkel->getAttribute ('col') : '(undefined)';

    my $spelling = $variable->getAttribute ('spelling');

    my $num_fraenkel_children = $fraenkel->findvalue ('count (*)');
    (my $fraenkel_term) = $fraenkel->findnodes ('*[position() = last() - 1]');
    (my $fraenkel_formula) = $fraenkel->findnodes ('*[position() = last()]');

    if (! defined $fraenkel_formula) {
	confess 'The Fraenkel-Term at (', $fraenkel_line, ',', $fraenkel_col, ' has no children.';
    }

    if (! defined $fraenkel_term) {
	confess 'The Fraenkel-Term at (', $fraenkel_line, ',', $fraenkel_col, ' has at most 1 child:', "\n", $fraenkel->toString ();
    }

    my @reserved_in_term = $fraenkel_term->findnodes ('descendant-or-self::Simple-Term[starts-with (@spelling, "R")]');

    warn 'checking whether the Fraenkel at (', $fraenkel_line, ',', $fraenkel_col, ') with ', $num_fraenkel_children, ' children binds ', $spelling;
    warn 'reserved in the term part of this Fraenkel:';
    foreach my $term (@reserved_in_term) {
	warn $term->getAttribute ('spelling');
    }

    my $reserved_idx = first_index { $_->getAttribute ('spelling') eq $spelling } @reserved_in_term;

    if ($reserved_idx < 0) {
	return 0;
    } else {
	my $reserved = $reserved_in_term[$reserved_idx];
	my $reserved_line = $reserved->getAttribute ('line');
	my $reserved_col = $reserved->getAttribute ('col');
	warn $spelling, ' appears to be bound by the item at (', $reserved_line, ',', $reserved_col, ').';
	return 1;
    }

}

sub node_binds_variable {
    my $node = shift;
    my $variable = shift;

    my $spelling = $variable->getAttribute ('spelling');

    if (is_fraenkel_term ($node)) {
	return fraenkel_binds_variable ($node, $variable);
    }

    my @binder_xpaths = (
	'self::Universal-Quantifier-Formula[Implicitly-Qualified-Segment/Variable[@spelling = "' . $spelling . '"]]',
	'self::Universal-Quantifier-Formula[Explicitly-Qualified-Segment/Variables/Variable[@spelling = "' . $spelling . '"]]',
	'self::Existential-Quantifier-Formula[Implicitly-Qualified-Segment/Variable[@spelling = "' . $spelling . '"]]',
	'self::Existential-Quantifier-Formula[Explicitly-Qualified-Segment/Variables/Variable[@spelling = "' . $spelling . '"]]',
	'self::Item[@kind = "Generalization" and Implicitly-Qualified-Segment/Variable[@spelling = "' . $spelling . '"]]',
	'self::Item[@kind = "Generalization" and Explicitly-Qualified-Segment/Variables/Variable[@spelling = "' . $spelling . '"]]',
	'self::Item[@kind = "Choice-Statement" and Implicitly-Qualified-Segment/Variable[@spelling = "' . $spelling . '"]]',
	'self::Item[@kind = "Choice-Statement" and Explicitly-Qualified-Segment/Variables/Variable[@spelling = "' . $spelling . '"]]',
	'self::Item[@kind = "Loci-Declaration" and Implicitly-Qualified-Segment/Variable[@spelling = "' . $spelling . '"]]',
	'self::Item[@kind = "Loci-Declaration" and Explicitly-Qualified-Segment/Variables/Variable[@spelling = "' . $spelling . '"]]',
	'self::Item[@kind = "Existential-Assumption" and Implicitly-Qualified-Segment/Variable[@spelling = "' . $spelling . '"]]',
	'self::Item[@kind = "Existential-Assumption" and Explicitly-Qualified-Segment/Variables/Variable[@spelling = "' . $spelling . '"]]',

    );

    if (any { $node->exists ($_) } @binder_xpaths) {
	return 1;
    } else {
	return 0;
    }

}

sub earlier_items {
    my $node = shift;

    my @preceding_siblings
	= $node->findnodes ('preceding-sibling::*');
    my $parent = $node->parentNode;

    if (defined $parent) {
	my @earlier_from_parent = earlier_items ($parent);
	return (@preceding_siblings, $parent, @earlier_from_parent);
    } else {
	return @preceding_siblings;
    }

}

sub bound_outside_containing_fraenkel {
    my $term = shift;
    (my $containing_fraenkel) = $term->findnodes ('ancestor::Fraenkel-Term');
    my @potential_binders = earlier_items ($containing_fraenkel);
    if (any { node_binds_variable ($_, $term) } @potential_binders) {
	return 1;
    } else {
	return 0;
    }
}

sub occurs_freely {
    my $variable_node = shift;

    my @potential_binders = earlier_items ($variable_node);

    my $binder_index = first_index { node_binds_variable ($_, $variable_node) } @potential_binders;

    if ($binder_index < 0) {
	return 1;
    } else {
	my $binder = $potential_binders[$binder_index];
	my $binder_line = $binder->getAttribute ('line');
	my $binder_col = $binder->getAttribute ('col');
	my $spelling = $variable_node->getAttribute ('spelling');
	warn $spelling, ' is bound by the item at (', $binder_line, ',', $binder_col, ').';
	return 0;
    }

}

sub bind_free_reserved_variables {
    my $formula_node = shift;
    my @exceptions = @_; # don't bind variables appearing here
    my @reserved = $formula_node->findnodes ('descendant::Simple-Term[starts-with (@spelling, "R")]');
    my @to_be_bound = ();
    warn 'bind_free_reserved_variables: ', scalar (@exceptions), ' exceptions';
    foreach my $variable (@reserved) {
	my $spelling = $variable->getAttribute ('spelling');
	my $line = $variable->getAttribute ('line');
	my $col = $variable->getAttribute ('col');
	warn 'considering ', $spelling, ' at (', $line, ',', $col, ')';
	if (any { $_->exists ('self::Explicitly-Qualified-Segment/Variables/*[@spelling = "' . $spelling . '"]') } @exceptions) {
	    warn 'dude, we have already taken care of ', $spelling;
	} else {
	    if (! (any { $_->getAttribute ('spelling') eq $spelling } @to_be_bound)) {
		if (occurs_freely ($variable)) {
		    push (@to_be_bound, $variable);
		    warn $spelling, ' at (', $line, ',', $col, ') appears to be free.';
		} else {
		    warn $spelling, ' appears to be already bound.';
		}
	    }
	}
    }

    my $document = $formula_node->ownerDocument;
    my $new_formula = $formula_node->cloneNode (1);

    @to_be_bound = reverse @to_be_bound;

    foreach my $variable (@to_be_bound) {
	my $spelling = $variable->getAttribute ('spelling');
	my $quantifier_element = new_element ('Universal-Quantifier-Formula',
					      $document);
	my $implicit_segment = new_element ('Implicitly-Qualified-Segment',
					    $document);
	my $variable_node = new_element ('Variable', $document);
	$variable_node->setAttribute ('spelling', $spelling);

	$implicit_segment->addChild ($variable_node);
	$quantifier_element->addChild ($implicit_segment);
	$quantifier_element->addChild ($new_formula);
	$new_formula = $quantifier_element;
    }

    return $new_formula;
}

sub explicitly_qualify_inner_fraenkels {
    my $term = shift;

    if (is_fraenkel_term ($term)) {
	my $transformed_fraenkel = explicitly_qualify_fraenkel ($term->cloneNode (1));
	warn 'HEY: we just transformed an inner fraenkel term.  Old', "\n", $term->toString, "\n", 'new:', "\n", $transformed_fraenkel->toString;
	return $transformed_fraenkel->cloneNode (1);
    } else {
	my @children = $term->childNodes;
	if (scalar @children == 0) {
	    return $term->cloneNode (1);
	} else {
	    my $term_name = $term->nodeName;
	    my $term_owner = $term->ownerDocument;
	    my $new_term = new_element ($term_name, $term_owner);
	    foreach my $child (@children) {
		my $transformed_child = explicitly_qualify_inner_fraenkels ($child);
		$new_term->addChild ($transformed_child);
	    }
	    return $new_term;
	}
    }
}

sub explicit_segment_less_than {
    my $segment_1 = shift;
    my $segment_2 = shift;

    (my $variable_1) = $segment_1->findnodes ('Variables/*[1]');
    (my $variable_2) = $segment_2->findnodes ('Variables/*[1]');

    if (! defined $variable_1) {
	confess 'Unable to extract a variable from a variable segment ', $segment_1->toString;
    }

    if (! defined $variable_2) {
	confess 'Unable to extract a variable from a variable segment ', $segment_2->toString;
    }

    return reserved_variable_less_than ($variable_1, $variable_2);

}

sub reserved_variable_less_than {
    my $var_1 = shift;
    my $var_2 = shift;

    my $var_1_name = $var_1->getAttribute ('spelling');
    my $var_2_name = $var_2->getAttribute ('spelling');

    if ($var_1_name =~ /\A R (\d+) \z/) {
	my $var_1_number = $1;
	if ($var_2_name =~ /\A R (\d+) \z/) {
	    my $var_2_number = $1;
	    if ($var_1_number < $var_2_number) {
		return -1;
	    } elsif ($var_2_number < $var_1_number) {
		return 1;
	    } else {
		return 0;
	    }
	} else {
	    confess 'Unable to make sense of variable spelling \'', $var_2_name, '\'.';
	}
    } else {
	    confess 'Unable to make sense of variable spelling \'', $var_1_name, '\'.';
    }

}

sub explicitly_qualify_fraenkel {
    my $fraenkel_node = shift;

    (my $fraenkel_term) = $fraenkel_node->findnodes ('*[position() = last() - 1]');
    (my $fraenkel_formula) = $fraenkel_node->findnodes ('*[position() = last()]');
    my @fraenkel_qualifiers = $fraenkel_node->findnodes ('*[position() < last() - 1]');

    my @reserved_in_term = $fraenkel_term->findnodes ('descendant-or-self::Simple-Term[starts-with (@spelling, "R")]');

    my %qualified = ();

    foreach my $qualified (@fraenkel_qualifiers) {
	# something should be done here
    }

    my $term_owner = $fraenkel_term->ownerDocument;

    my @new_qualifiers = ();

    foreach my $term (@reserved_in_term) {
	my $term_spelling = $term->getAttribute ('spelling');
	warn 'explicitly_qualify_fraenkel: considering ', $term_spelling;
	if (is_fraenkel_term ($fraenkel_term)) {
	    if (fraenkel_binds_variable ($fraenkel_term, $term)) {
		warn 'fraenkel term DOES bind ', $term_spelling, '; what should we do here?';
	    } elsif (! defined $qualified{$term_spelling}) {
		warn 'WOW! We are going to qualify ', $term_spelling;
		my $explicit_segment_node =
		    new_element ('Explicitly-Qualified-Segment', $term_owner);
		my $variables_node = new_element ('Variables', $term_owner);
		my $type = reserved_variable_type ($term);
		$variables_node->addChild ($term->cloneNode (1));
		$explicit_segment_node->addChild ($variables_node);
		$explicit_segment_node->addChild ($type->cloneNode (1));
		push (@new_qualifiers, $explicit_segment_node);
		$qualified{$term_spelling} = 0;
	    } else {
		# nothing to say or do
	    }
	} elsif (! defined $qualified{$term_spelling}) {
	    if (! bound_outside_containing_fraenkel ($term)) {
		my $explicit_segment_node =
		    new_element ('Explicitly-Qualified-Segment', $term_owner);
		my $variables_node = new_element ('Variables', $term_owner);
		my $type = reserved_variable_type ($term);
		$variables_node->addChild ($term->cloneNode (1));
		$explicit_segment_node->addChild ($variables_node);
		$explicit_segment_node->addChild ($type->cloneNode (1));
		push (@new_qualifiers, $explicit_segment_node);
		$qualified{$term_spelling} = 0;
	    }
	}
    }

    @new_qualifiers = sort { explicit_segment_less_than ($a, $b) } @new_qualifiers;

    @fraenkel_qualifiers = (@new_qualifiers, @fraenkel_qualifiers);

    my $new_fraenkel = new_element ('Fraenkel-Term', $term_owner);
    foreach my $qualifier (@fraenkel_qualifiers) {
	$new_fraenkel->addChild ($qualifier->cloneNode (1));
    }

    # warn 'about to possibly qualify the term part of the Fraenkel term ', $fraenkel_node->toString ();

    my $new_fraenkel_term = element_node_without_reservations ($fraenkel_term);
    $new_fraenkel->addChild ($new_fraenkel_term->cloneNode (1));
    $fraenkel_node->replaceChild ($new_fraenkel_term->cloneNode (1), $fraenkel_term);
    warn 'new fraenkel term:', "\n", $new_fraenkel_term->toString;


    # $new_fraenkel->addChild ($fraenkel_term->cloneNode (1));

    # warn 'about to bind free variables in the formula part of the Fraenkel term ', $fraenkel_node->toString ();

    my $transformed_formula = bind_free_reserved_variables ($fraenkel_formula, @fraenkel_qualifiers);

    warn 'replacing child of ', $fraenkel_node->toString ();
    $fraenkel_node->replaceChild ($transformed_formula,
     				  $fraenkel_formula);
    my $no_res_transformed =
     	element_node_without_reservations ($transformed_formula);
     $new_fraenkel->addChild ($no_res_transformed);

    # $new_fraenkel->addChild ($fraenkel_formula);

    return $new_fraenkel;
}

sub reserved_variable_type {
    my $variable = shift;
    my $spelling = $variable->getAttribute ('spelling');
    (my $earlier_reservation) = $variable->findnodes ('preceding::Item[@kind = "Reservation" and Variables/Variable[@spelling = "' . $spelling . '"][1]]');
    if (! defined $earlier_reservation) {
	confess 'We found no previous reservation for ', $spelling;
    }

    my $earlier_line = $earlier_reservation->getAttribute ('line');
    my $earlier_col = $earlier_reservation->getAttribute ('col');

    warn $spelling, ' was reserved at line ', $earlier_line, ' and col ', $earlier_col;

    (my $earlier_type) = $earlier_reservation->findnodes ('*[position() = last()]');

    if (! defined $earlier_type) {
	confess 'Unable to determine the reserved type for ', $spelling;
    }

    return $earlier_type;
}

sub implicit_to_explicit {
    my $node = shift;
    my $node_line = $node->getAttribute ('line');
    my $node_col = $node->getAttribute ('col');
    my @variables = $node->findnodes ('Variable');
    my @explicit_segments = ();
    foreach my $variable (@variables) {
	my $explicit_element
	    = XML::LibXML::Element->new ('Explicitly-Qualified-Segment');
	$explicit_element->setOwnerDocument ($node->ownerDocument ());
	my $variables_element = XML::LibXML::Element->new ('Variables');
	$variables_element->setOwnerDocument ($node->ownerDocument ());

	my $earlier_type = reserved_variable_type ($variable);

	$variables_element->addChild ($variable->cloneNode (1));
	$explicit_element->addChild ($variables_element->cloneNode (1));
	$explicit_element->addChild ($earlier_type->cloneNode (1));
	push (@explicit_segments, $explicit_element);
    }

    return @explicit_segments;

}

sub element_node_without_reservations {
    my $element_node = shift;
    my $element_name = $element_node->nodeName ();

    my $new_element_node = $element_node->cloneNode (0);
    my @children = $element_node->childNodes ();
    foreach my $child (@children) {
	if (is_reservation_node ($child)) {
	    # skip
	} elsif (is_implicitly_qualified_segment ($child)) {
	    my @explicit_children = implicit_to_explicit ($child);
	    foreach my $explicit_child (@explicit_children) {
		$new_element_node->addChild ($explicit_child);
	    }
	} elsif (is_fraenkel_term ($child)) {
	    my $explicit_fraenkel = explicitly_qualify_fraenkel ($child);
	    $new_element_node->addChild ($explicit_fraenkel);
	} else {
	    my $transformed_child = node_without_reservations ($child);
	    $new_element_node->addChild ($transformed_child);
	}
    }
    return $new_element_node;
}

sub node_without_reservations {
    my $node = shift;

    my $type = $node->nodeType ();

    if ($type == XML::LibXML::XML_TEXT_NODE) {
	return text_node_without_reservations ($node);
    } elsif ($type == XML::LibXML::XML_ELEMENT_NODE) {
	return element_node_without_reservations ($node);
    } else {
	confess 'Unknown node type ', $type;
    }
}

sub without_reservations {
    my $self = shift;

    my $styesheet_home = $self->get_stylesheet_home ();
    my $name = $self->name ();
    my $path = $self->get_path ();
    my $tpr_path = $self->file_with_extension ('tpr');

    my $wrm_stylesheet = $self->path_for_stylesheet ('wrm');
    my $pp_stylesheet = $self->path_for_stylesheet ('wsm');
    my $msm_path = $self->file_with_extension ('msm');

    if (! ensure_readable_file ($wrm_stylesheet)) {
	confess ('The without-reservations stylesheet could not be found at the expected location', $LF, $LF, '  ', $wrm_stylesheet, $LF);
    }

    $self->accom ();
    $self->msplit ();
    $self->wsmparser ();
    $self->msmprocessor ();
    File::Copy::copy ($msm_path, $tpr_path)
	  or confess ('Could not copy ', $msm_path, ' to ', $tpr_path, ': ', $!);
    $self->mglue ();
    $self->accom ();
    $self->wsmparser ();

    my $wsx_path = $self->file_with_extension ('wsx');
    if (! ensure_readable_file ($wsx_path)) {
	confess ('The .wsx file for ', $name, ' does not exist (or is unreadable).');
    }

    my $wsx_doc = eval { $xml_parser->parse_file ($wsx_path) };

    if (! defined $wsx_doc) {
	confess 'Unable to parse ', $wsx_path, ' as XML.';
    }

    my $wsx_root = $wsx_doc->documentElement ();
    my $new_wsx_root = node_without_reservations ($wsx_root);
    my $new_wsx_doc = XML::LibXML::Document->createDocument ();

    $new_wsx_doc->setDocumentElement ($new_wsx_root);

    my $temp = File::Temp->new ();
    my $wsx_doc_string = $new_wsx_doc->toString;

    warn 'wsx doc string:', "\n", $wsx_doc_string;

    my $temp_path = $temp->filename;
    write_string_to_file ($wsx_doc_string, $temp_path);

    my $new_text = apply_stylesheet ($pp_stylesheet,
				     $temp_path,
				     $tpr_path,
				     { 'suppress-environment' => '1' });

    $self->mglue ();
    $self->wsmparser ();

    return 1;

}

1;
