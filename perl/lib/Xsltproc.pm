package Xsltproc;

use base qw(Exporter);
use warnings;
use strict;
use Carp qw(croak);
use IPC::Run qw(run);

our @EXPORT_OK = qw(apply_stylesheet);

sub apply_stylesheet {
    my $stylesheet = shift;
    my $document = shift;
    my $result_path = shift;
    my $parameters_ref = shift;

    my %parameters = defined $parameters_ref ? %{$parameters_ref}
	                                     : ()
					     ;

    my @xsltproc_call = ('xsltproc');
    my $params = '';
    foreach my $parameter (keys %parameters) {
	my $value = $parameters{$parameter};
	push (@xsltproc_call, '--stringparam', $parameter, "'${value}'");
    }
    push (@xsltproc_call, $stylesheet);
    push (@xsltproc_call, $document);

    my $xsltproc_out = '';
    my $xsltproc_err = '';

    my $xsltproc_result = run (\@xsltproc_call,
			       '>',
			       defined $result_path ? $result_path
				                    : \$xsltproc_out
						    ,
			       '2>',
			       \$xsltproc_err);

    if ($xsltproc_result == 0) {
	croak ('Error: xsltproc did not exit cleanly when applying the stylesheet ', $stylesheet, ' to ', $document, '.  Here is the error output: ', "\n", $xsltproc_err);
    } else {
	if (defined $result_path) {
	    return 1;
	} else {
	    if (wantarray) {
		chomp $xsltproc_out;
		return split (/\n/, $xsltproc_out);
	    } else {
		return $xsltproc_out;
	    }
	}
    }

}

1; # I'm OK, you're OK
