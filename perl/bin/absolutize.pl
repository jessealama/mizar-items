#!/usr/bin/perl

use strict;
use warnings;

use Pod::Usage;
use File::Basename qw(basename dirname);
use Getopt::Long;

use FindBin;
use lib '/Users/alama/sources/mizar/mizar-items/perl/lib';

use Article;
use Utils qw(ensure_readable_file);

my $help = 0;
my $man = 0;
GetOptions (
    'help' => \$help,
    'man' => \$man,
) or pod2usage(2);
pod2usage(1) if $help;
pod2usage(-exitstatus => 0, -verbose => 2) if $man;

pod2usage(1) if (scalar @ARGV != 1);

my $article = $ARGV[0];
my $article_dirname = dirname ($article);
my $article_basename = basename ($article, '.miz');
my $article_miz = "${article_dirname}/${article_basename}.miz";

if (! ensure_readable_file ($article_miz)) {
    print 'Error: there is no .miz file at ', $article_miz, ' (or it is unreadable).', "\n";
    exit 1;
}

my $a = Article->new (path => $article_miz);

$a->absolutize ();
$a->absolutize_environment ();

exit 0;

__END__
