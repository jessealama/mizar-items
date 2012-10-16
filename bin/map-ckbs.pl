#!/usr/bin/perl

use warnings;
use strict;
use File::Basename qw(basename);
use Getopt::Long;
use Pod::Usage;

use FindBin qw($RealBin);
use lib "$RealBin/../src/perl";
use LocalDatabase;
use ItemizedArticle;
use Utils qw(ensure_directory);

my $help = 0;
my $man = 0;
my $verbose = 0;
my $stylesheet_home = "$RealBin/../src/xslt";
my $script_home = "$RealBin";

GetOptions('help|?' => \$help,
           'man' => \$man,
           'stylesheet-home' => \$stylesheet_home,
           'script-home' => \$script_home,
           'verbose'  => \$verbose)
    or pod2usage(2);
pod2usage(1) if $help;
pod2usage(-exitstatus => 0, -verbose => 2) if $man;
pod2usage(1) if (scalar @ARGV != 1);

my $article_dir = $ARGV[0];

if (! -d $article_dir) {
    print 'Error: the supplied directory \'', $article_dir, '\' does not exist.', "\n";
    exit 1;
}

if (! -d $stylesheet_home) {
    print 'Errior: the supplied directory \'', $stylesheet_home, '\' to be used for locating stylesheets is not a directory.', "\n";
    exit 1;
}

if (! -d $script_home) {
    print 'Errior: the supplied directory \'', $stylesheet_home, '\' to be used for locating scripts is not a directory.', "\n";
    exit 1;
}

my $local_db = LocalDatabase->new (location => $article_dir,
			           stylesheet_home => $stylesheet_home,
			           script_home => $script_home);
my $itemized_article = ItemizedArticle->new (local_database => $local_db,
					     stylesheet_home => $stylesheet_home,
					     script_home => $script_home);

my %item_to_fragment_table = %{$itemized_article->get_item_to_fragment_table ()};

foreach my $key (keys %item_to_fragment_table) {
    print $key, ' => ', $item_to_fragment_table{$key}, "\n";
}

__END__

=head1 MAP-CKBS

map-ckbs.pl - Resolve names of article-internal items ("CKBs")

=head1 SYNOPSIS

map-ckbs.pl [options] directory

Interpret the supplied directory as the result of itemizing a Mizar
article.  Print a mapping that shows how each of the items in the
itemized article is resolved into its absolute name.

=head1 OPTIONS

=over 8

=item B<--help>

Prints a brief help message and exits.

=item B<--man>

Prints the manual page and exits.

=item B<--verbose>

Say what we're doing as we're doing it.

=back

=head1 DESCRIPTION

B<map-ckbs.pl> will consult the given article directory to determine
how any "article-internal" items are resolved to their absolute names.

=head1 REQUIRED ARGUMENTS

It is necessary to supply a directory as the one and only argument of
this program.  The directory is supposed to be the result of itemizing
a Mizar article.  It should have the structure of a multi-article
Mizar development: there should be subdirectories 'prel', 'dict', and
'text'.

=head1 SEE ALSO

=over 8

=item F<itemized-article-dependencies.pl>

=item L<http://mizar.org/>

=back

=cut
