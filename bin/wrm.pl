#!/usr/bin/perl

use strict;
use warnings;

use Getopt::Long;
use File::Temp qw(tempfile);
use File::Basename qw(basename dirname);
use File::Spec;
use File::Copy qw(copy);
use XML::LibXML;
use Cwd qw(cwd);
use File::Copy qw(copy move);
use Carp qw(croak carp);
use IPC::Cmd qw(can_run);
use Pod::Usage;
use charnames qw(:full);
use Readonly;

Readonly my $LF => "\N{LF}";

use FindBin qw($RealBin);

my $opt_man = 0;
my $opt_help = 0;
my $opt_verbose = 0;
my $opt_debug = 0;
my $opt_check = 0;

GetOptions (
    'man' => \$opt_man,
    'verbose' => \$opt_verbose,
    'help' => \$opt_help,
    'check' => \$opt_check,
) or pod2usage (-exitval => 2);

if (@ARGV != 1) {
    pod2usage (-exitval => 2);
}

my $make_dir = "$RealBin/../src/make";

unless (-d $make_dir) {
    say {*STDERR} 'Error: the makefile directory', $LF, $LF, '  ', $make_dir, $LF, $LF, 'is not a directory.';
    exit 1;
}

my $wrm_makefile = "${make_dir}/wrm.makefile";

if (-d $wrm_makefile) {
    say {*STDERR} 'Error: the WRM makefile is actually a directory!';
    exit 1;
}

unless (-e $wrm_makefile) {
    say {*STDERR} 'Error: the WRM makefile does not exist.';
    exit 1;
}

unless (-r $wrm_makefile) {
    say {*STDERR} 'Error: the WRM makefile is not readable.';
    exit 1;
}

my $article_arg = $ARGV[0];
my $article_basename = basename ($article_arg, '.miz');
my $article_dirname = dirname ($article_arg);
my $article_miz = "${article_basename}.miz";
my $article_wrm = "${article_basename}.wrm";
my $article_tpr = "${article_basename}.tpr";
my $article_verifier_stamp = "${article_basename}.verifier-stamp";

system ('make', "--makefile=${wrm_makefile}", $article_wrm);

if ($opt_check) {

    system ('make', "--makefile=${wrm_makefile}", $article_tpr);
    copy ($article_wrm, $article_tpr);
    system ('mglue', $article_miz);
    system ('make', "--makefile=${wrm_makefile}", $article_verifier_stamp);

}

__END__
