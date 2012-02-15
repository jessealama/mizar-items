package Xsltproc;

use base qw(Exporter);
use warnings;
use strict;
use Carp qw(croak carp);
use IPC::Run qw(start);
use Data::Dumper;

use Utils qw(ensure_readable_file);

our @EXPORT = qw(apply_stylesheet);

sub apply_stylesheet {

    my ($stylesheet, $document, $result_path, $parameters_ref) = @_;

    if (! defined $stylesheet) {
	croak ('Error: please supply a stylesheet.');
    }

    if (! defined $document) {
	croak ('Error: please supply a document.');
    }

    if (! ensure_readable_file ($document)) {
	croak ('Error: there is no file at ', $document, '.');
    }

    my %parameters = defined $parameters_ref ? %{$parameters_ref}
	                                     : ()
					     ;

    my @xsltproc_call = ('xsltproc');
    my $params = '';
    foreach my $parameter (keys %parameters) {
	my $value = $parameters{$parameter};
	push (@xsltproc_call, '--stringparam', $parameter, $value);
    }
    if (defined $result_path) {
	push (@xsltproc_call, '--output', $result_path);
    }

    push (@xsltproc_call, $stylesheet);
    push (@xsltproc_call, $document);

    # warn 'xsltproc_call = ', Dumper (@xsltproc_call);

    my $xsltproc_out = '';
    my $xsltproc_err = '';

    my $xsltproc_harness = start (\@xsltproc_call,
				  '>', \$xsltproc_out,
				  '2>', \$xsltproc_err);

    $xsltproc_harness->finish ();
    my $xsltproc_result = $xsltproc_harness->result (0);

    if ($xsltproc_result != 0) {
	croak ('Error: xsltproc did not exit cleanly when applying the stylesheet', "\n", "\n", '  ', $stylesheet, "\n", "\n", 'to', "\n", "\n", '  ', $document, ' .  Its exit code was ', $xsltproc_result, '.', "\n", "\n",  'Here is the error output: ', "\n", $xsltproc_err);
    }

    if (defined $result_path) {
	return 1;
    } elsif (wantarray) {
	# carp 'HEY: wantarray; xsltproc output is ', $xsltproc_out;
	chomp $xsltproc_out;
	my @answer = split (/\n/, $xsltproc_out);
	return @answer;
    } else {
	# carp 'HEY: do not wantarray';
	return $xsltproc_out;
    }

}

1; # I'm OK, you're OK
