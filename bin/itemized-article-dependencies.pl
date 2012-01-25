#!/usr/bin/perl

use warnings;
use strict;
use File::Basename qw(basename);
use Getopt::Long;
use Pod::Usage;
use File::Temp qw(tempfile);
use Carp qw(croak carp);
use XML::LibXML;
use Memoize qw(memoize);
use Regexp::DefaultFlags;
use IPC::Run qw(run);
use charnames qw( :full ); # for referring to characters in regular expressions

use FindBin;
use lib "$FindBin::Bin";

use Utils qw(ensure_readable_file ensure_directory ensure_executable);

# Set up an XML parser that we might use
my $xml_parser = XML::LibXML->new ();

my $stylesheet_home = undef;
my $script_home = undef;
my $verbose = 0;
my $man = 0;
my $help = 0;

GetOptions('help|?' => \$help,
           'man' => \$man,
           'verbose'  => \$verbose,
	   'stylesheet-home=s' => \$stylesheet_home,
	   'script-home=s', \$script_home)
    or pod2usage(2);
pod2usage(1) if $help;
pod2usage(-exitstatus => 0, -verbose => 2) if $man;
pod2usage(1) if (scalar @ARGV != 1);


if (! defined $stylesheet_home) {
    $stylesheet_home = '/Users/alama/sources/mizar/xsl4mizar/items';
}

if (! defined $script_home) {
    $script_home = '/Users/alama/sources/mizar/xsl4mizar/items';
}

my $article_dir = $ARGV[0];

sub ensure_sensible_commandline {

    if (not ensure_directory ($article_dir)) {
	croak ('Error: the supplied directory', "\n", "\n", '  ', $article_dir, "\n", "\n", 'does not exist.');
    }

    if (not ensure_directory ($stylesheet_home)) {
	croak ('Error: the supplied directory', "\n", "\n", '  ', $stylesheet_home, "\n", "\n", 'in which we look for stylesheets could not be found.');
    }

    if (not ensure_directory ($script_home)) {
	croak ('Error: the supplied directory', "\n", "\n", '  ', $script_home, "\n", "\n", 'in which we look for auxiliary scripts could not be found.');
    }

    return 1;
}

ensure_sensible_commandline ();

my $article_basename = basename ($article_dir);

sub path_for_stylesheet {
    my $sheet = shift;
    my $path  = "${stylesheet_home}/${sheet}.xsl";
    if (ensure_readable_file ($path)) {
	return $path;
    } else {
        croak( 'Error: the ', $sheet,
	       ' stylesheet does not exist at the expected location (',
	       $path, ') or is unreadable.' );
    }
}

sub path_for_script {
    my $script = shift;
    my $path  = "${script_home}/${script}";
    if (ensure_executable ($script)) {
	return $path;
    } else {
        croak( 'Error: the ', $script,
	       ' script does not exist at the expected location (',
	       $path, ') or is not readable and executable.' );
    }
}

my %item_to_fragment_table = ();
my %fragment_to_item_table = ();

sub load_fragment_item_tables {

    my $map_ckb_script = path_for_script ('map-ckbs.pl');
    my @map_ckb_call = ($map_ckb_script, $article_dir);
    my $map_ckb_err = '';
    my $map_ckb_out = '';
    run (\@map_ckb_call,
	 '>', \$map_ckb_out,
	 '2>', \$map_ckb_err)
	or croak ('Error: something went wrong computing the item-to-fragment table.', "\n", , 'Here is the error output produced by the map-ckb script:', "\n", $map_ckb_err);

    chomp $map_ckb_out;
    my @item_to_fragment_lines = split ("\n", $map_ckb_out);
    chomp @item_to_fragment_lines;
    if (scalar @item_to_fragment_lines == 0) {
	carp ('Warning: there are 0 fragments under ', $article_dir, '.');
    }

    foreach my $item_to_fragment_line (@item_to_fragment_lines) {
	if ($item_to_fragment_line =~ /\A ([a-z0-9_]+ : [^:]+ : [0-9]+ ( \[ [a-z]+ \] )?) [\N{SPACE}] => [\N{SPACE}] ([a-z0-9_]+ : fragment : [0-9]+ (\[ [a-z]+ \])?) \z/) {
	    (my $item, my $fragment) = ($1, $3);
	    $item_to_fragment_table{$item} = $fragment;
	    $fragment_to_item_table{$fragment} = $item;
	} else {
	    croak ('Error: Unable to make sense of the mapping \'', $item_to_fragment_line, '\'.');
	}
    }

    return 1;

}

load_fragment_item_tables ();


memoize ('resolve_item');
sub resolve_item {
    my $item = shift;

    if ($item =~ /\A ckb ([0-9]+) : ([^:]+) : [0-9]+ /) {
	(my $item_fragment_num, my $item_kind) = ($1, $2);

	my $item_fragment = undef;

	if ($item =~ / (.) pattern : /) {
	    my $pattern_kind = $1;
	    $item_fragment = "${article_basename}:fragment:${item_fragment_num}[${pattern_kind}p]";
	} elsif ($item =~ / (.) definiens /) {
	    my $definiens_kind = $1;
	    $item_fragment = "${article_basename}:fragment:${item_fragment_num}[${definiens_kind}f]";
	} elsif ($item =~ / deftheorem /) {
	    $item_fragment = "${article_basename}:fragment:${item_fragment_num}[dt]";
	} elsif ($item =~ / \[ existence \] / ) {
	    $item_fragment = "${article_basename}:fragment:${item_fragment_num}[ex]";
	} elsif ($item =~ / (.) constructor /) {
	    my $constructor_kind = $1;
	    $item_fragment = "${article_basename}:fragment:${item_fragment_num}[${constructor_kind}c]";
	} else {
	    $item_fragment = "${article_basename}:fragment:${item_fragment_num}";
	}

	if (defined $fragment_to_item_table{$item_fragment}) {

	    return $fragment_to_item_table{$item_fragment};

	} else {
	    croak ('Error: the fragment-to-item table does not contain ', $item_fragment, '.', "\n", 'The keys of the fragment-to-item table are:', "\n", join ("\n", keys %fragment_to_item_table), "\n");
	}
    } else {
	return $item;
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
       'pattern' => 'pa');

my %full_name_of_shortcut = ();
foreach my $long_name (keys %conditions_and_properties_shortcuts) {
    my $shortcut = $conditions_and_properties_shortcuts{$long_name};
    $full_name_of_shortcut{$shortcut} = $long_name;
}

# Esnure that function constructors that lack existence and uniqueness
# conditions, but do have a coherence condition, generate existence
# and uniqueness items that depend on the coherence condition

foreach my $item (keys %item_to_fragment_table) {
    if ($item =~ / : kconstructor : [0-9]+ \z / ) {
	my $existence_condition = "${item}[existence]";
	my $uniqueness_condition = "${item}[uniqueness]";
	if (! defined $item_to_fragment_table{$existence_condition}
		&& ! defined $item_to_fragment_table{$uniqueness_condition}) {
	    my $coherence_condition = "${item}[coherence]";
	    if (defined $item_to_fragment_table{$coherence_condition}) {
		print $existence_condition, ' ', $coherence_condition, "\n";
		print $uniqueness_condition, ' ', $coherence_condition, "\n";
	    } else {
		croak ('Error: the function constructor ', $item, ' lacks known existence and uniqueness conditions, as well as a known coherence condition.', "\n");
	    }
	}
    }
}

# Deal with properties copied into redefined constructors.  Let's
# declare that they depend on the original constructor property.

memoize ('fragment_is_redefined_constructor');
sub fragment_is_redefined_constructor {
    my $fragment = shift;
    if ($fragment =~ / : fragment : ([0-9]+) /) {
	my $fragment_number = $1;
	my $fragment_abs_xml = "${article_dir}/text/ckb${fragment_number}.xml1";
	if (-e $fragment_abs_xml) {
	    if (defined (my $fragment_doc = $xml_parser->parse_file ($fragment_abs_xml))) {
		(my $constructor) = $fragment_doc->findnodes ('.//Constructor[@redefaid and @absredefnr]');
		defined $constructor ? return 1
		    : return 0;
	    } else {
		croak ('Error: the XML file at ', $fragment_abs_xml, ' is either unreadable or contains invalid XML.');
	    }
	} else {
	    croak ('Error: unable to make sense of the fragment \'', $fragment, '\'.', "\n");
	}
    }
}

memoize ('original_constructor');
sub original_constructor {
    my $fragment = shift;
    if ($fragment =~ / : fragment : ([0-9]+) /) {
	my $fragment_number = $1;
	my $fragment_abs_xml = "${article_dir}/text/ckb${fragment_number}.xml1";
	if (ensure_readable_file ($fragment_abs_xml)) {
	    if (defined (my $fragment_doc = $xml_parser->parse_file ($fragment_abs_xml))) {
		(my $constructor) = $fragment_doc->findnodes ('.//Constructor[@redefaid and @absredefnr and @kind]');
		if (defined $constructor) {
		    my $kind = $constructor->findvalue ('@kind');
		    my $kind_lc = lc $kind;
		    my $redefaid = $constructor->findvalue ('@redefaid');
		    my $redefaid_lc = lc $redefaid;
		    my $absredefnr = $constructor->findvalue ('@absredefnr');
		    return "${redefaid_lc}:${kind_lc}constructor:${absredefnr}";
		} else {
		    croak ('Error: we assumed that the fragment ', $fragment, ' is a redefinition with a new constructor, but it seems not to be.', "\n");
		}
	    } else {
		croak ('Error: the XML file at ', $fragment_abs_xml, ' either does not exist or contains invalid XML.');
	    }
	} else {
	    croak ('Error: the absolute form of ', $fragment, ' does not exist.');
	}
    } else {
	croak ('Error: unable to make sense of the fragment \'', $fragment, '\'.', "\n");
    }
}

foreach my $item (keys %item_to_fragment_table) {
    if ($item =~ / : (.) constructor : [0-9]+ \[ ([a-z]+) \] \z /) {
	my $property = $2;
	if ($property ne 'coherence') {
	    my $fragment = $item_to_fragment_table{$item};
	    if (fragment_is_redefined_constructor ($fragment)) {
		my $original = original_constructor ($fragment);
		my $original_resolved = resolve_item ($original);
		print $item, ' ', $original_resolved, '[', $property, ']', "\n";
	    }
	}
    }
}

sub miz_path_for_item {

    my $item = shift;

    my $path = undef;

    if ($item =~ /\A ([a-z0-9_]+) : ([^:]+) : ([0-9]+) /) {
	(my $item_article, my $item_kind, my $item_number) = ($1, $2, $3);
	my $fragment = $item_to_fragment_table{$item};
	if ($fragment =~ m/\A ${article_basename} : fragment : ([0-9]+) /) {
	    my $fragment_number = $1;
	    # Possibly resolve properties and correctness conditions
	    if ($fragment =~ / \[ ([a-z]+) \] \z /) {
		my $property_or_condition_code = $1;
		$path = "${article_dir}/text/ckb${fragment_number}${property_or_condition_code}.miz";
	    } elsif ($item =~ / : (.) constructor [0-9]+ \z /) {
		my $constructor_kind = $1;
		$path = "${article_dir}/text/ckb${fragment_number}${constructor_kind}c.miz";
	    } else {
		$path = "${article_dir}/text/ckb${fragment_number}.miz";
	    }
	} else {
	    croak ('Error: we could not extract the article fragment number from the text', "\n", '  ', $fragment);
	}

    } else {
	croak ('Error: we cannot make sense of the item \'', $item, '\'.', "\n");
    }

    if (-e $path) {
	return $path;
    } else {
	return undef;
    }
}

# Build a list of files on which we will compute their dependencies

my $dependencies_script = path_for_script ('dependencies.pl');

# my @items = keys %item_to_fragment_table;
# my @item_paths = map { miz_path_for_item ($_); } @items;

# my @parallel_call = ('parallel',
# 		     '--keep',
# 		     "/bin/echo -n '{}: '; ${dependencies_script} --stylesheet-home=${stylesheet_home} --oneline {}; /bin/echo",
# 		     ':::',
# 		     @item_paths);

# my $parallel_out = '';
# my $parallel_err = '';
# run (\@parallel_call,
#      '>', \$parallel_out,
#      '2>', \$parallel_err)
#     or croak ('Error: parallel did not exit cleanly when computing the dependencies of the fragments for ', $article_basename, '.  The error output was:', "\n", $parallel_err);

# my @unresolved_deps = split ("\n", $parallel_out);

# # DEBUG
# warn 'Wow: parallel output (nresolved deps) was:', "\n", $parallel_out;
# warn 'There are ', scalar @unresolved_deps, ' dep lines';

# if (scalar @unresolved_deps == scalar @items) {
#     warn 'Holy crap, they are equal!';
# }
# else {
#     warn 'Rats.';
# }


foreach my $item (keys %item_to_fragment_table) {

    my %item_deps = ();

    # Ensure that functor constructors depend directly on their
    # corresponding existence and uniqueness conditions.

    if ($item =~ / : kconstructor : [0-9]+ \z /) {
	my $existence_item = "${item}[existence]";
	my $uniqueness_item = "${item}[uniqueness]";
	$item_deps{$existence_item} = 0;
	$item_deps{$uniqueness_item} = 0;
    }

    my $item_miz = miz_path_for_item ($item);

    if (-e $item_miz) { # in the case of redefinitions, there may be
                        # no .miz for $item
	my $dependencies_out = '';
	my $dependencies_err = '';
	my @dependencies_call = ($dependencies_script, "--stylesheet-home=${stylesheet_home}", $item_miz);
	run (\@dependencies_call,
	     '>', \$dependencies_out,
	     '2>', \$dependencies_err)
	    or croak ('Error: the article dependencies script did not exit cleanly whe working on the item ', $item_miz, ' of ', $article_basename, '; the error output was: ', "\n", $dependencies_err);

	chomp $dependencies_out;
	my @fragment_dependencies = split ("\n", $dependencies_out);
	chomp @fragment_dependencies;
	foreach my $dep (@fragment_dependencies) {
	    my $resolved_dep = eval { resolve_item ($dep); };
	    my $resolve_err = $@;
	    if (! defined $resolved_dep) {
		croak ('Error: we were unable to resolve the dependency ', $dep, ' of the item ', $item, '.', "\n", 'The reported error was:', "\n", $resolve_err);
	    }
	    $item_deps{$resolved_dep} = 0;

	}
    }

    # Now print the dependencies
    print $item;
    foreach my $dep (keys %item_deps) {
	print ' ', $dep;
    }
    print "\n";

}

__END__

=head1 ITEMIZED-ARTICLE-DEPENDENCIES

itemized-article-dependencies.pl - Print the dependencies of an itemized Mizar article

=head1 SYNOPSIS

itemized-article-dependencies.pl [options] directory

Interpret the supplied directory as the result of itemizing a Mizar
article, and print the dependencies of each of the microarticles under
the supplied directory.

=head1 OPTIONS

=over 8

=item B<--help>

Prints a brief help message and exits.

=item B<--man>

Prints the manual page and exits.

=item B<--verbose>

Say what we're doing as we're doing it.

=item B<--script-home=DIR>

The directory in which we will look for the needed auxiliary scripts.

=item B<--stylesheet-home=DIR>

The directory in which we will look for any needed auxiliary
stylesheets.

=back

=head1 DESCRIPTION

B<itemized-article-dependencies.pl> will consult the given article as
well as its environment to determine the article's dependencies, which
it prints (one per line) to standard output.

=head1 REQUIRED ARGUMENTS

It is necessary to supply a directory as the one and only argument of
this program.  The directory is supposed to be the result of itemizing
a Mizar article.  It should have the structure of a multi-article
Mizar development: there should be subdirectories 'prel', 'dict', and
'text'.

=head1 SEE ALSO

=over 8

=item F<dependencies.pl>

=item L<http://mizar.org/>

=back

=cut
