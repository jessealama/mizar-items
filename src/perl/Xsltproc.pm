package Xsltproc;

use base qw(Exporter);
use warnings;
use strict;
use Carp qw(croak carp);
use XML::LibXSLT;
use XML::LibXML;
use charnames qw(:full);
use Readonly;
use Regexp::DefaultFlags;
use charnames qw(:full);

our @EXPORT = qw(apply_stylesheet);

Readonly my $RECURSION_DEPTH => 3000;
Readonly my $LF => "\N{LF}";

XML::LibXSLT->max_depth ($RECURSION_DEPTH);

sub print_message {
    my $msg = shift;
    warn $msg;
    return;
}

XML::LibXSLT->debug_callback (\&print_message);

my $xml_parser = XML::LibXML->new ();

my %parsed_stylesheet_table = ();

sub apply_stylesheet {

    my ($stylesheet, $document, $result_path, $parameters_ref) = @_;

    if (! defined $stylesheet) {
	croak ('Error: please supply a stylesheet.');
    }

    if (! defined $document) {
	croak ('Error: please supply a document.');
    }

    my %parameters = XML::LibXSLT::xpath_to_string
	(defined $parameters_ref ? %{$parameters_ref} : () );

    my $parsed_stylesheet = $parsed_stylesheet_table{$stylesheet};
    if (! defined $parsed_stylesheet) {
	my $xslt = XML::LibXSLT->new ();
	my $style_doc = XML::LibXML->load_xml (location => $stylesheet);
	$parsed_stylesheet = $xslt->parse_stylesheet ($style_doc);
	$parsed_stylesheet_table{$stylesheet} = $parsed_stylesheet;
    }

    my ($results, $err);
    if ($document =~ /[\N{LF}]/m) {
	# this isn't a file
	my $doc = $xml_parser->load_xml (string => $document);
	$results = eval { $parsed_stylesheet->transform ($doc,
							 %parameters) };
	$err = $!;
    } elsif (-e $document && -r $document) {
	$results = eval { $parsed_stylesheet->transform_file ($document,
							      %parameters) };
	$err = $!;
    } else {
	# $document appears to be a string
	my $doc = $xml_parser->load_xml (string => $document);
	$results = eval { $parsed_stylesheet->transform ($document,
							 %parameters) };
	$err = $!;
    }

    if (! defined $results) {
	if (defined $err) {
	    croak 'Unable to apply ', $stylesheet, '.  The error was:', $LF, $LF, $err, $LF;
	} else {
	    croak 'Unable to apply ', $stylesheet, '.  It is unclear what the error was.';
	}
    }

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
