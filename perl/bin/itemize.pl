#!/usr/bin/perl

# Check first that our environment is sensible

use strict;
use warnings;
use Getopt::Long;
use File::Temp qw(tempfile);
use File::Basename qw(basename dirname);
use File::Spec;
use XML::LibXML;
use Cwd qw(cwd);
use File::Copy qw(copy move);
use Carp qw(croak carp);
use IPC::Cmd qw(can_run);

use lib '/Users/alama/sources/mizar/mizar-items/perl/lib';
use Utils qw(ensure_directory ensure_readable_file extension strip_extension);
use Mizar;
use Article;
use LocalDatabase;
use Xsltproc qw(apply_stylesheet);

my $paranoid = 0;
my $stylesheet_home = '/Users/alama/sources/mizar/mizar-items';
my $verbose = 0;
my $debug = 0;
my $man = 0;
my $help = 0;
my $target_directory = undef;

GetOptions('help|?' => \$help,
           'man' => \$man,
           'verbose'  => \$verbose,
	   'debug' => \$debug,
	   'paranoid' => \$paranoid,
	   'stylesheet-home=s' => \$stylesheet_home,
	   'target-directory=s' => \$target_directory)
  or pod2usage(2);
pod2usage(1) if $help;
pod2usage(-exitstatus => 0, -verbose => 2) if $man;
pod2usage(1) unless (scalar @ARGV == 1);

my $article_path = $ARGV[0];
my $article_basename = basename ($article_path, '.miz');
my $article_dirname = dirname ($article_path);
my $article_miz = "${article_dirname}/${article_basename}.miz";

if (! ensure_readable_file ($article_miz)) {
    croak ('Error: ', $article_miz, ' does not exist (as a file) or is not readable.');
}

Mizar::set_stylesheet_home ($stylesheet_home);

if (defined $target_directory) {
    if (-e $target_directory) {
	croak ('Error: the supplied target directory, ', $target_directory, ' already exists.  Please move it out of the way.', "\n");
    }
} else {
    my $cwd = cwd ();
    $target_directory = "${cwd}/${article_basename}";
    if (-e $target_directory) {
	croak ('Error: since the target-directory option was not used, we are to save our wok in \'', $article_basename, '\'; but there is already a directory by that name in the current working directory.  Please move it out of the way.', "\n");
    }
}

my $article = Article->new (path => $article_miz);
$article->itemize ($target_directory);

__END__

=pod

=encoding utf8

=head1 NAME

itemize.pl - Divide a mizar article into fragments

=head1 USAGE

itemize.pl [options] mizar-article

=head1 REQUIRED ARGUMENTS

A Mizar article, as a path, must be supplied.

=head1 CONFIGURATION

This program requires that a working Mizar installation is available.
We require these Mizar programs:

=over 8

=item verifier

=item accom

=item msplit

=item mglue

=item exporter

=item transfer

=item wsmparser

=item msmprocessor

=back

The first six are a part of the standard distribution, but wsmparser
and msmprocessor are (as of 2012-01-03) not part of the standard
release.  Please contact the author or the mizar-forum mailing list
(L<mizar-forum@mizar.uwb.edu.pl|mailto:mizar-forum@mizar.uwb.edu.pl>)
to obtain suitable versions of wsmparser and msmprocessor.

=head1 ENVIRONMENT

The MIZFILES environment variable needs to be set to a sensible value.

=head1 DEPENDENCIES

=over 8

=item B<Perl dependencies>

=over 8

=item Cwd

=item File::Basename

=item File::Copy qw(copy);

=item Getopt::Long

=item Pod::Usage

=item XML::LibXML

=back

=item B<XSL dependencies>

This package requires that xsltproc be available.  These stylesheets,
in addition, are required:

=over 8

=item F<split.xsl>

=item F<itemize.xsl>

=item F<wsm.xsl>

=item F<extend-evl.xsl>

=back

If you do not have these stylesheets, see
L<the github page for this program and related Mizar code|https://github.com/jessealama/xsl4mizar/tree/master/items/>
to obtain the latest versions of them.  Use the --stylesheet-home option to
specify the directory in which to look for these needed stylesheets.

=back

=head1 OPTIONS

=over 8

=item B<--help>

Prints a brief help message and exits.

=item B<--man>

Prints the manual page and exits.

=item B<--verbose>

Say what we're doing.

=item B<--paranoid>

After itemizing, verify all of the article's fragments.

=item B<--target-directory=DIR>

Save our work in the specified directory.  It is an error if the
specified directory already exists.

=item B<--stylesheet-home=DIR>

The directory in which we will look for any needed stylesheets.

=back

=head1 DESCRIPTION

B<itemize.pl> will divide a mizar article into fragments.  The
fragments will be stored in the directory indicated by the
--target-directory option.  If that option is not supplied, we will
attempt to create, in the working directory in which this script is
called, a directory whose name is the basename of the supplied
article.  It is an error if this directory, or the directory supplied
by the --target-directory option, already exists.

=head1 EXIT STATUS

0 if everything went OK, 1 if any error occurred.  No other exit codes
are used.

=head1 DIAGNOSTICS

Before doing anything, we inspect the MIZFILES environment variable,
and that certain needed Mizar programs are available.  If MIZFILES is
not set, or set to a strange value, we will terminate.  If any of the
needed Mizar programs is not found, we will terminate.

We also check whether the xsltproc XSLT processor is available.  If it
is unavailable, we cannot proceed.

This program, when run with no options on a Mizar article that can be
entirely itemized, will generate no output.  Before itemizing the
article, it is copied to a new directory (whose creation can fail).

The article is then rewritten using various transformations by the
xsltproc XLST processor.  If any of these transformations fails, this
program will terminate and one will learn that some step (which
involves the Mizar accom, verifier, msplit, mglue, wsmparser, and
msmprocessor programs, as well as a handful of XSL stylesheets) has
failed.  At present we do not pass along whatever error messages
xsltproc generated, nor do we explain any Mizar error files.  If a
Mizar program fails, you can see for yourself how it failed by
consulting the .err file corresponding to the failing .err file; see
also the file F<mizar.msg> under the MIZFILES environment variable.

=head1 BUGS AND LIMITATIONS

Sending a signal to this program when it is writing out the fragments
of the initial article and running accom/verifier/exporter/transfer
should probably kill the whole process.  Instead, it kills
accom/verifier/exporter/transfer, and itemization continues in an
incoherent state.

There are some opportunities for parallelization of the itemization
process, but at the moment we are not exploiting these.

=head1 SEE ALSO

=over 8

=item F<itemized-article-dependencies.pl>

=item L<http://mizar.org/>

=back

=head1 INCOMPATIBILITIES

None known.

=head1 AUTHOR

Jesse Alama <jesse.alama@gmail.com>

=head1 LICENSE AND COPYRIGHT

This source is offered under the terms of
L<the GNU GPL version 3|http://www.gnu.org/licenses/gpl-3.0.en.html>.

=cut
