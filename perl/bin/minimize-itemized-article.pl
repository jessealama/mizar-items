#!/usr/bin/perl

use strict;
use warnings;
use File::Basename qw(basename dirname);
use File::Copy qw(copy move);
use File::Copy::Recursive qw(dircopy dirmove);
use Getopt::Long;
use Pod::Usage;
use File::Temp qw(tempdir);
use Carp qw(croak);

use FindBin qw($RealBin);
use lib "$RealBin/../lib";
use Utils qw(ensure_directory ensure_readable_file ensure_executable);
use LocalDatabase;

my $verbose = 0;
my $debug = 0;
my $man = 0;
my $help = 0;
my $paranoid = 0;
my $minimize_whole_article = 0;
my $script_home = "$RealBin/../../bin";
my $stylesheet_home = "$RealBin/../../xsl";
my $nice = 0;
my $num_jobs = undef;
my $workdir = undef;

GetOptions('help|?' => \$help,
           'man' => \$man,
           'verbose'  => \$verbose,
	   'debug' => \$debug,
           'minimize-whole-article' => \$minimize_whole_article,
	   'script-home=s' => \$script_home,
	   'stylesheet-home=s' => \$stylesheet_home,
	   'nice' => \$nice,
	   'paranoid' => \$paranoid,
	   'jobs=i' => \$num_jobs,
	   'workdir=s' => \$workdir)
  or pod2usage(2);
pod2usage(1) if $help;
pod2usage(-exitstatus => 0, -verbose => 2) if $man;

if (defined $num_jobs) {
  if ($num_jobs < 1) {
    pod2usage(1);
  }
}

if (scalar @ARGV != 1) {
    pod2usage (1);
}

if (defined $workdir) {
  ensure_directory ($workdir)
      or croak ('Error: the work directory does not exist.');
}

my $article_dir = $ARGV[0];
my $local_db = LocalDatabase->new (location => $article_dir,
			           stylesheet_home => $stylesheet_home);
my $itemized_article = ItemizedArticle->new (local_database => $local_db,
					     stylesheet_home => $stylesheet_home);

$itemized_article->minimize ();

__END__

=pod

=head1 minimize-itemized-article.pl

minimize-itemized-article.pl - Minimize the fragments of an "itemized" mizar article

=head1 SYNOPSIS

minimize-itemized-article.pl [options] itemized-mizar-article-directory

=head1 OPTIONS

=over 8

=item B<--help>

Prints a brief help message and exits.

=item B<--man>

Prints the manual page and exits.

=item B<--verbose>

Say what environment file we're minimizing, and for each environment
file, say how many environment "items" are present there and how many
we really need.

=item B<--minimize-whole-article>

Before minimizing the environment of each of the items from an
article, minimize the whole article and use its minimal environment as
a basis for each of the article's item's environment.

Such minimization may an expensive up-front cost, but it can save time
in the long run, depending on the size of the original article's
environment and how expensive it is to verify it.

=item B<--paranoid>

Call the verifier on each of the itemized articles.

=item B<--script-home=DIR>

The directory in which we will look for any needed scripts.

=item B<--stylesheet-home=DIR>

The directory in which we will look for any needed stylesheets.

=item B<--nice>

Use nice when minimizing the original article's fragments.

=item B<--jobs=NUM-JOBS>

Run NUM-JOBS fragment minimization jobs in parallel.  By default, all
available processors will be used.

=item B<--workdir=DIR>

Do the minimization in DIR (e.g., a ramdisk).  This means that before
anything else, the directory to be minimized will be copied to DIR.
Upon normal completion, the original direcory will be deleted and the
contents of the newly minimized directory will be moved into the
original directory.  If something goes wrong during the minimization,
the original directory will not be deleted, and the itemized article
subdirectory of DIR will be deleted.

=back

=head1 DESCRIPTION

B<minimize-itemized-article.pl> will construct, for each fragment of
an itemized mizar article, the smallest environment with respect to
which the given article is verifiable.

=cut
