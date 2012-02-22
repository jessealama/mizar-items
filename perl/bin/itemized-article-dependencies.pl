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

use FindBin qw($RealBin);
use lib "$RealBin/../lib";

use Utils qw(ensure_readable_file ensure_directory ensure_executable);
use ItemizedArticle;

# Set up an XML parser that we might use
my $xml_parser = XML::LibXML->new ();

my $stylesheet_home = "$RealBin/../../xsl";
my $script_home = "$RealBin/../../bin";
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


my $article_dir = $ARGV[0];

sub ensure_sensible_commandline {

    if (! ensure_directory ($article_dir)) {
	croak ('Error: the supplied directory', "\n", "\n", '  ', $article_dir, "\n", "\n", 'does not exist.');
    }

    if (! ensure_directory ($stylesheet_home)) {
	croak ('Error: the supplied directory', "\n", "\n", '  ', $stylesheet_home, "\n", "\n", 'in which we look for stylesheets could not be found.');
    }

    if (! ensure_directory ($script_home)) {
	croak ('Error: the supplied directory', "\n", "\n", '  ', $script_home, "\n", "\n", 'in which we look for auxiliary scripts could not be found.');
    }

    return 1;
}

ensure_sensible_commandline ();

if (! ensure_directory ($article_dir)) {
    print 'Error: there is no directory at \'', $article_dir, '\'.', "\n";
    exit 1;
}

my $itemized_article = ItemizedArticle->new (location => $article_dir,
					     stylesheet_home => $stylesheet_home);

my %dependencies = %{$itemized_article->dependencies ()};

foreach my $item (keys %dependencies) {
    my @deps = @{$dependencies{$item}};
    print $item;
    foreach my $dep (@deps) {
	print ' ', $dep;
    }
    print "\n";
}

exit 0;

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
