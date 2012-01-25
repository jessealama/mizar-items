#!/usr/bin/perl

use strict;
use warnings;

# Set up our location
use FindBin;
use lib "$FindBin::Bin";

use Getopt::Long;
use Pod::Usage;
use File::Basename qw(basename dirname);
use Carp qw(croak);
use Regexp::DefaultFlags;
use Xsltproc qw(apply_stylesheet);
use Utils qw(ensure_directory);

my $stylesheet_home = undef;
my $verbose         = 0;
my $debug           = 0;
my $man             = 0;
my $help            = 0;
my $one_line_output = 0;

GetOptions(
    'help|?'            => \$help,
    'man'               => \$man,
    'verbose'           => \$verbose,
    'debug'             => \$debug,
    'oneline'           => \$one_line_output,
    'stylesheet-home=s' => \$stylesheet_home
) or pod2usage(2);

if ($help) {
    pod2usage(1);
}

if ($man) {
    pod2usage(
        -exitstatus => 0,
        -verbose    => 2
    );
}

if ( scalar @ARGV != 1 ) {
    pod2usage(1);
}

if ( !defined $stylesheet_home ) {
    $stylesheet_home = '/Users/alama/sources/mizar/xsl4mizar/items';
}

ensure_directory($stylesheet_home);

my $article = $ARGV[0];

my $article_basename = basename( $article, '.miz' );
my $article_dirname = dirname($article);

sub mizar_file_with_extension {
    my $extension = shift;
    return "${article_dirname}/${article_basename}.${extension}";
}

my $article_atr          = mizar_file_with_extension('atr');
my $article_xml          = mizar_file_with_extension('xml');
my $article_absolute_xml = mizar_file_with_extension('xml1');

my $dependencies_stylesheet = path_for_stylesheet('dependencies');
my $absrefs_stylesheet      = path_for_stylesheet('addabsrefs');

sub ensure_absolute_xml {

    if ( !-e $article_xml ) {
        croak( 'Error: the semantic XML for ',
            $article_basename, ' does not exist.' );
    }

    if ( !-e $article_absolute_xml ) {

        if ($verbose) {
            print 'The absolute form of the XML for ', $article_basename,
                ' does not exist.  Generating...';
        }

        apply_stylesheet( $absrefs_stylesheet, $article_xml,
            $article_absolute_xml );

        if ($verbose) {
            print 'done.', "\n";
        }
    }

    return 1;
}

sub path_for_stylesheet {
    my $sheet = shift;
    my $path  = "${stylesheet_home}/${sheet}.xsl";
    if ( -e $path ) {
        if ( -r $path ) {
            return $path;
        }
        else {
            croak( 'Error: the ', $sheet, ' stylesheet at ',
                $path, ' is unreadable.' );
        }
    }
    else {
        croak( 'Error: the ', $sheet,
            ' stylesheet does not exist at the expected location (',
            $path, ').' );
    }
}

ensure_absolute_xml();

# Ensure that the absolute forms of the article's environment XMLs are
# available
my @extensions = (
    qw{eno}, qw{erd}, qw{epr}, qw{dfs}, qw{eid}, qw{ecl}, qw{esh}, qw{eth},
);
foreach my $extension (@extensions) {
    my $env_xml     = mizar_file_with_extension($extension);
    my $env_abs_xml = mizar_file_with_extension("${extension}1");
    if ( -e $env_xml && !-e $env_abs_xml ) {
        apply_stylesheet( $absrefs_stylesheet, $env_xml, $env_abs_xml );
    }
}

my @non_constructor_deps =
    apply_stylesheet( $dependencies_stylesheet, $article_absolute_xml );

# Constructors are a special case
my $inferred_constructors_stylesheet =
    path_for_stylesheet('inferred-constructors');

my @inferred_constructors =
    apply_stylesheet( $inferred_constructors_stylesheet,
    $article_absolute_xml );

my %deps_table = ();

foreach ( @non_constructor_deps, @inferred_constructors ) {
    $deps_table{$_} = 0;
}

# Print constructor property dependencies

my $properties_of_constructor_stylesheet =
    path_for_stylesheet('properties-of-constructor');

foreach my $constructor (@inferred_constructors) {
    if ( $constructor
        =~ /\A ([ _ [:lower:] \d ]+) : (.) constructor : (\d+) \z/ )
    {
        ( my $aid, my $kind, my $nr ) = ( $1, $2, $3 );
        my %stylesheet_parameters = (
            'kind' => $kind,
            'nr'   => $nr,
            'aid'  => $aid,
        );
        my @properties =
            apply_stylesheet( $properties_of_constructor_stylesheet,
            $article_atr, undef, \%stylesheet_parameters );

        foreach my $property (@properties) {
            my $property_lc  = lc $property;
            my $property_key = "${constructor}[${property_lc}]";
            $deps_table{$property_key} = 0;
        }
    }
    else {
        croak( 'Error: unable to make sense of the constructor \'',
            $constructor, '\'.' );
    }
}

if ($one_line_output) {
    my @items = keys %deps_table;
    if (@items) {
	print $items[0];
	shift @items;
	foreach my $item (@items) {
	    print ' ', $item;
	}
    }
}
else {
    foreach ( keys %deps_table ) {
	print $_, "\n";
    }
}

__END__

=head1 DEPENDENCIES

dependencies.pl - Print the dependencies of a Mizar article

=head1 SYNOPSIS

dependencies.pl [options] mizar-article

=head1 OPTIONS

=over 8

=item B<--help>

Prints a brief help message and exits.

=item B<--man>

Prints the manual page and exits.

=item B<--verbose>

Say what we're doing as we're doing it.

=item B<--debug>

Print copious amounts of information for debugging purposes.

=back

=head1 DESCRIPTION

B<dependencies.pl> will consult the given article as well as its
environment to determine the article's dependencies, which it prints
(one per line) to standard output.

=cut
