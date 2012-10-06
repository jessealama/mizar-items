package Xsltproc;

use base qw(Exporter);
use warnings;
use strict;
use Carp qw(croak carp);
use Data::Dumper;
use XML::LibXSLT;
use XML::LibXML;
use charnames qw(:full);

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

    my $xslt = XML::LibXSLT->new ();
    my $style_doc = XML::LibXML->load_xml (location => $stylesheet);
    my $parsed_stylesheet = $xslt->parse_stylesheet ($style_doc);
    my $results = $parsed_stylesheet->transform_file ($document, %parameters);
    my $result_bytes = $parsed_stylesheet->output_as_bytes ($results);

    if (defined $result_path) {
	open (my $result_fh, '>', $result_path);
	print {$result_fh} $result_bytes;
	close $result_fh;
    }

    if (wantarray) {
	return split ("\N{LF}", $result_bytes);
    } else {
	return $result_bytes;
    }

}

1; # I'm OK, you're OK
