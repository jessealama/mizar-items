#!/usr/bin/perl

use strict;
use warnings;
use Carp qw(carp croak confess);
use Pod::Usage;
use Getopt::Long;
use XML::LibXML;
use XML::LibXSLT;
use File::Basename;
use File::Temp;
use File::Copy;
use Regexp::DefaultFlags;
use Readonly;
use charnames qw(:full);

Readonly my $LF => "\N{LF}";

my $opt_help = 0;
my $opt_man = 0;

Getoptions (
    'help' => \$opt_help,
    'man' => \$opt_man
) or pod2usage (-exitval => 2);

if (scalar @ARGV == 0) {
    pod2usage (-exitval => 2,
               -message => 'Error: mandatory article argument missing.');
}

if (scalar @ARGV > 1) {
    pod2usage (-exitval => 2,
               -message => 'Error: too many arguments.');
}

if ($opt_man) {
    pod2usage (-verbose => 2,
               -exitval => 0);
}

if ($opt_help) {
    pod2usage (-verbose => 1,
               -exitval => 0);
}

my $article = $ARGV[0];
my $article_dir = dirname ($article);
my $article_basename = basename ($article);
my $article_miz = "${article_basename}.miz"
my $article_miz_full = "${article_dir}/${article_miz}";

if (! -e $article_miz) {
    confess 'No such file \'', $article_miz, '\'.';
}

if (-d $article_miz) {
    confess $article_miz, ' is actually a directory.';
}

if (! -r $article_miz) {
    confess $article_miz, ' is unreadable.';
}

my $tempdir = tempdir (CLEANUP => 1);
my $xml_parser = XML::LibXML->new ();

my $article_miz_in_tempdir = "${tempdir}/${article_miz}";

copy ($article_miz_full, $article_miz_in_tempdir)
    or confess 'Unable to copy', $LF, $LF, $SP, $SP, $article_miz_full, $LF, $LF, 'to', $LF, $LF, $SP, $SP, $article_miz_in_tempdir, $LF, $LF, $!;

exit 0;

__END__

=head1 NAME

descheme.pl

=head1 SYNOPSIS

desceheme.pl ARTICLE

=head1 DESCRIPTION

C<descheme.pl> removes instantiates all uses of schemes in a Mizar article.
If, in a proof inside the given article, one finds a statement justified by a scheme

X from SCHEME:1(A,B);

one replaces this use by a toplevel unexported theorem.  All schemes
go all the way back, ultimately, to the axiom scheme of replacement.
