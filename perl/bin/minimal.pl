#!/usr/bin/perl

use warnings;
use strict;
use File::Basename qw(basename dirname);
use XML::LibXML;
use POSIX qw(floor ceil);
use Getopt::Long;
use Pod::Usage;
use Carp qw(croak);
use IPC::Run qw(run);

use FindBin;
use lib '/Users/alama/sources/mizar/mizar-items/perl/lib';

use Utils qw(ensure_directory ensure_readable_file ensure_executable);
use Article;

my $paranoid = 0;
my $verbose = 0;
my $debug = 0;
my $man = 0;
my $help = 0;
my $confirm_only = 0;
my $checker_only = 0;
my $script_home = "$FindBin::Bin";
my $fast_theorems = 0;
my $fast_schemes = 0;

GetOptions('help|?' => \$help,
           'man' => \$man,
           'verbose'  => \$verbose,
	   'debug' => \$debug,
	   'paranoid' => \$paranoid,
	   'fast-schemes' => \$fast_schemes,
	   'script-home' => \$script_home,
	   'fast-theorems' => \$fast_theorems,
	   'checker-only' => \$checker_only,
	   'confirm-only' => \$confirm_only)
  or pod2usage(2);
pod2usage(1) if $help;
pod2usage(-exitstatus => 0, -verbose => 2) if $man;

pod2usage(1) unless (scalar @ARGV == 1);

ensure_directory ($script_home);

my $article = $ARGV[0];
my $article_basename = basename ($article, '.miz');
my $article_dirname = dirname ($article);
my $article_miz = "${article_dirname}/${article_basename}.miz";

my $a = Article->new (path => $article_miz);

$a->minimize ( { 'checker-only' => $checker_only } );

__END__

=cut

=head1 minimal.pl

minimal.pl - Minimize the environment of a mizar article

=head1 SYNOPSIS

minimize.pl [options] mizar-article

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

=item B<--debug>

Be very verbose about what is going on, for debugging purposes.

=item B<--paranoid>

Before minimizing, check that the article is verifiable.  If it is,
the continue, otherwise exit uncleanly.  After minimization of the
article's environment, check again that it is verifiable.  If it
isn't, then exit uncleanly.

=item B<--confirm-only>

Don't do any minimization, but check that the environment really is
minimal in the sense that there is no item from any environment file
(except the .atr file) that can be deleted while still preserving
verifiability.

=back

=head1 DESCRIPTION

B<minimize.pl> will construct, in a brute-force manner, the smallest
environment with respect to which the given article is verifiable.

=cut
