#!/usr/bin/perl

use strict;
use warnings;

require v5.10; # for the 'say' feature;
use feature 'say';
use Data::Dumper;
use File::Basename qw(basename);
use Regexp::DefaultFlags;
use Getopt::Long;
use Pod::Usage;
use XML::LibXML;
use FindBin qw($RealBin);
use lib "$RealBin/../src/perl";
use Xsltproc qw(apply_stylesheet);

my $fragment_prefix = 'ckb';

sub list_as_token_string {
  return ',' . join (',', @_) . ',';
}

sub ensure_readable_file {
    my $path = shift;
    if (-d $path) {
	say {*STDERR} $path, ' is a directory, not a file.';
	exit 1;
    }
    if (! -e $path) {
	say {*STDERR} $path, ' does not exist.';
	exit 1;
    }
    if (! -r $path) {
	say {*STDERR} $path, ' is unreadable.';
	exit 1;
    }
    return;
}

sub fragment_less_than {
    my $fragment_1 = shift;
    my $fragment_2 = shift;

    my $fragment_number_1;
    my $fragment_number_2;
    if ($fragment_1 =~ /\A ${fragment_prefix} (\d+) \z /) {
	$fragment_number_1 = $1;
    } else {
	say {*STDERR} 'Unable to make sense of \'', $fragment_1, '\'.';
	exit 1;
    }
    if ($fragment_2 =~ /\A ${fragment_prefix} (\d+) \z /) {
	$fragment_number_2 = $1;
    } else {
	say {*STDERR} 'Unable to make sense of \'', $fragment_2, '\'.';
	exit 1;
    }

    if ($fragment_number_1 < $fragment_number_2) {
	return -1;
    } elsif ($fragment_number_2 < $fragment_number_1) {
	return 1;
    } else {
	return 0;
    }
}

my $stylesheet_home = "$RealBin/../src/xslt";

if (! -d $stylesheet_home) {
    say {*STDERR} 'We need to look for stylesheets under ', $stylesheet_home, ', but that is not a directory.';
}

my $extend_evl_stylesheet = "${stylesheet_home}/extend-evl.xsl";
my $evl2environ_stylehseet = "${stylesheet_home}/evl2environ.xsl";

ensure_readable_file ($extend_evl_stylesheet);
ensure_readable_file ($evl2environ_stylehseet);

GetOptions(
    # nothing yet
) or pod2usage (-exitval => 2);

if (scalar @ARGV < 1) {
    pod2usage (-exitval => 2);
}

my $evl = shift @ARGV;
my @prels = @ARGV;

ensure_readable_file ($evl);

my $xml_parser = XML::LibXML->new ();
my $evl_doc = eval { $xml_parser->parse_file ($evl) };

if (! defined $evl_doc) {
    say {*STDERR} $evl, ' appears not to be a valid XML file.';
    exit 1;
}

foreach my $prel (@prels) { ensure_readable_file ($prel); }

@prels = map { basename ($_) } @prels;

my %prels_by_extension = ();

foreach my $prel (@prels) {
    if ($prel =~ /\A ${fragment_prefix} (\d+) [.] ([a-z]{3}) \z/) {
	my ($number, $extension) = ($1, $2);
	$prels_by_extension{$extension}{"${fragment_prefix}${number}"} = 0;
    } else {
	say {*STDERR} 'Cannot make sense of \'', $prel, '\'.';
	exit 1;
    }
}

my %dnos = defined $prels_by_extension{'dno'}
    ? %{$prels_by_extension{'dno'}}
    : ();
my %drds = defined $prels_by_extension{'drd'}
    ? %{$prels_by_extension{'drd'}}
    : ();
my %dcls = defined $prels_by_extension{'dcl'}
    ? %{$prels_by_extension{'dcl'}}
    : ();
my %dids = defined $prels_by_extension{'did'}
    ? %{$prels_by_extension{'did'}}
    : ();
my %defs = defined $prels_by_extension{'def'}
    ? %{$prels_by_extension{'def'}}
    : ();
my %thes = defined $prels_by_extension{'the'}
    ? %{$prels_by_extension{'the'}}
    : ();
my %schs = defined $prels_by_extension{'sch'}
    ? %{$prels_by_extension{'sch'}}
    : ();
my %dcos = defined $prels_by_extension{'dco'}
    ? %{$prels_by_extension{'dco'}}
    : ();

my @dnos = keys %dnos;
my @drds = keys %drds;
my @dcls = keys %dcls;
my @dids = keys %dids;
my @defs = keys %defs;
my @thes = keys %thes;
my @schs = keys %schs;
my @dcos = keys %dcos;

my @constructors = @dcos;
my @notations = @dnos;
my @theorems = @thes;
my @schemes= @schs;
my @registrations = (@drds, @dcls, @dids);
my @definitions = @defs;

@constructors = sort { fragment_less_than ($a, $b) } @constructors;
@notations = sort { fragment_less_than ($a, $b) } @notations;
@theorems = sort { fragment_less_than ($a, $b) } @theorems;
@schemes = sort { fragment_less_than ($a, $b) } @schemes;
@registrations = sort { fragment_less_than ($a, $b) } @registrations;
@definitions = sort { fragment_less_than ($a, $b) } @definitions;

@constructors = map { uc $_ } @constructors;
@notations = map { uc $_ } @notations;
@theorems = map { uc $_ } @theorems;
@schemes = map { uc $_ } @schemes;
@registrations = map { uc $_ } @registrations;
@definitions = map { uc $_ } @definitions;

my $constructors_token_string = list_as_token_string (@constructors);
my $notations_token_string = list_as_token_string (@notations);
my $theorems_token_string = list_as_token_string (@theorems);
my $schemes_token_string = list_as_token_string (@schemes);
my $registrations_token_string = list_as_token_string (@registrations);
my $definitions_token_string = list_as_token_string (@definitions);

my $new_evl = apply_stylesheet
    ($extend_evl_stylesheet,
     $evl,
     undef,
     {
	 'constructors' => $constructors_token_string,
	 'notations' => $notations_token_string,
	 'theorems' => $theorems_token_string,
	 'schemes' => $schemes_token_string,
	 'registrations' => $registrations_token_string,
	 'definitions' => $definitions_token_string,
     }
 );

my $new_evd = apply_stylesheet ($evl2environ_stylehseet,
				$new_evl);

print $new_evd;

exit $?;

__END__
