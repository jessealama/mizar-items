#!/usr/bin/perl

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
use Pod::Usage;
use charnames qw(:full);
use Readonly;

use FindBin qw($RealBin);
use lib "$RealBin/../src/perl";
use Utils qw(ensure_directory ensure_readable_file extension strip_extension);
use Mizar;
use Article;
use Xsltproc qw(apply_stylesheet);

Readonly my $LF => "\N{LF}";

my $opt_man = 0;
my $opt_help = 0;
my $opt_verbose = 0;
my $opt_debug = 0;
my $opt_check = 0;

my $stylesheet_home = "$RealBin/../src/xslt";
my $script_home = $RealBin;

if (! -d $stylesheet_home) {
    say {*STDERR} 'The stylesheet home directory at', $LF, $LF, '  ', $stylesheet_home, $LF, $LF, 'is not actually a directory.';
    exit 1;
}

GetOptions (
    'man' => \$opt_man,
    'verbose' => \$opt_verbose,
    'help' => \$opt_help,
    'check' => \$opt_check,
) or pod2usage (-exitval => 2);

if (@ARGV != 1) {
    pod2usage (-exitval => 2);
}

my $article_arg = $ARGV[0];
my $article_basename = basename ($article_arg, '.miz');
my $article_dirname = dirname ($article_arg);
my $article_miz = "${article_basename}.miz";

if (! ensure_readable_file ($article_miz)) {
    say {*STDERR} 'There is no readable file at ', $article_miz, '.';
    exit 1;
}

my $article = Article->new (path => $article_miz,
			    stylesheet_home => $stylesheet_home,
			    script_home => $script_home);

$article->without_reservations ();

if ($opt_check) {

    if ($article->accom ()) {
	if ($article->is_verifiable ()) {
	    exit 0;
	} else {
	    say {*STDERR} 'The WRM form of ', $article_miz, ' could not be verified.';
	    exit 1;
	}
    } else {
	say {*STDERR} 'The WRM form of ', $article_miz, ' could not be accommodated.';
	exit 1;
    }


}

__END__
