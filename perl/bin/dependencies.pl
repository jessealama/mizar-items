#!/usr/bin/perl

use strict;
use warnings;

# Set up our location
use FindBin;
use lib '/Users/alama/sources/mizar/mizar-items/perl/lib';
use Xsltproc qw(apply_stylesheet);
use Utils qw(ensure_directory);
use Article;

use Getopt::Long;
use Pod::Usage;
use File::Basename qw(basename dirname);
use Carp qw(croak);
use Regexp::DefaultFlags;

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

my $article_path = $ARGV[0];
my $article_basename = basename ($article_path, '.miz');
my $article_dirname = dirname ($article_path);
my $article_miz = "${article_dirname}/${article_basename}.miz";

my $article = Article->new (path => $article_miz);

my @needed = $article->needed_items ();

if ($one_line_output) {
    if (@needed) {
	print $needed[0];
	shift @needed;
	foreach (@needed) {
	    print ' ', $_;
	}
    }
}
else {
    foreach ( @needed ) {
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
