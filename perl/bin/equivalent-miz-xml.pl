#!/usr/bin/perl

use strict;
use warnings;
use XML::LibXML;
use Pod::Usage;
use Getopt::Long;

sub text_nodes_are_equal {
    my $text_node_1 = shift;
    my $text_node_2 = shift;

    my $text_1 = $text_node_1->nodeValue ();
    my $text_2 = $text_node_2->nodeValue ();

    return ($text_1 eq $text_2);
}

my @uninteresting_attributes = ('pid',
				'relnr',
				'redefnr',
				'line',
				'col',
				'x',
				'y',
				'mizfiles');

my %uninteresting_attrs = ();
foreach my $attr (@uninteresting_attributes) {
    $uninteresting_attrs{$attr} = 0;
}

sub attribute_nodes_are_equal {
    my $attr_node_1 = shift;
    my $attr_node_2 = shift;

    my $attr_name_1 = $attr_node_1->nodeName ();
    my $attr_name_2 = $attr_node_2->nodeName ();

    if ($attr_name_1 eq $attr_name_2) {
	if (defined $uninteresting_attrs{$attr_name_1}) {
	    return 1;
	} else {
	    my $attr_value_1 = $attr_node_1->getValue ();
	    my $attr_value_2 = $attr_node_2->getValue ();
	    if ($attr_value_1 eq $attr_value_2) {
		return 1;
	    } else {
		warn 'Different attribute values: \'', $attr_value_1, '\' vs \'', $attr_value_2, '\'.';
		return 0;
	    }
	}
    } else {
	warn 'Different attribute names: \'', $attr_name_1, '\' vs \'', $attr_name_2, '\'.';
	return 0;
    }

}

sub nodes_have_same_attributes {
    my $node_1 = shift;
    my $node_2 = shift;
    my @attrs_1 = $node_1->attributes ();
    my @attrs_2 = $node_2->attributes ();
    my %attr_1_table = ();
    my %attr_2_table = ();
    foreach my $attr (@attrs_1) {
	my $attr_name = $attr->nodeName ();
	if (! defined $uninteresting_attrs{$attr_name}) {
	    my $attr_value = $attr->value ();
	    $attr_1_table{$attr_name} = $attr_value;
	}
    }
    foreach my $attr (@attrs_2) {
	my $attr_name = $attr->nodeName ();
	if (! defined $uninteresting_attrs{$attr_name}) {
	    my $attr_value = $attr->value ();
	    $attr_2_table{$attr_name} = $attr_value;
	}
    }
    foreach my $attr_name (keys %attr_1_table) {
	if (defined $attr_2_table{$attr_name}) {
	    my $attr_1_value = $attr_1_table{$attr_name};
	    my $attr_2_value = $attr_2_table{$attr_name};
	    if ($attr_1_value ne $attr_2_value) {
		return 0;
	    }
	} else {
	    return 0;
	}
    }
    foreach my $attr_name (keys %attr_2_table) {
	if (defined $attr_1_table{$attr_name}) {
	    my $attr_1_value = $attr_1_table{$attr_name};
	    my $attr_2_value = $attr_2_table{$attr_name};
	    if ($attr_1_value ne $attr_2_value) {
		return 0;
	    }
	} else {
	    return 0;
	}
    }
    return 1;
}

sub element_nodes_are_equal {
    my $element_node_1 = shift;
    my $element_node_2 = shift;

    my $element_name_1 = $element_node_1->nodeName ();
    my $element_name_2 = $element_node_2->nodeName ();

    if ($element_name_1 eq $element_name_2) {
	if (nodes_have_same_attributes ($element_node_1, $element_node_2)) {
	    my @children_1 = $element_node_1->childNodes ();
	    my @children_2 = $element_node_2->childNodes ();
	    my $num_children_1 = scalar @children_1;
	    my $num_children_2 = scalar @children_2;
	    if ($num_children_1 == $num_children_2) {
		foreach my $i (0 .. $num_children_1 - 1) {
		    my $child_1 = $children_1[$i];
		    my $child_2 = $children_2[$i];
		    if (! nodes_are_equal ($child_1, $child_2)) {
			return 0;
		    }
		}
		return 1;
	    } else {
		warn 'Differing number of children: ', $num_children_1, ' vs ', $num_children_2, '.';
		return 0;
	    }
	} else {
	    return 0;
	}
    }
}

sub nodes_are_equal {
    my $xml_node_1 = shift;
    my $xml_node_2 = shift;

    my $type_1 = $xml_node_1->nodeType ();
    my $type_2 = $xml_node_2->nodeType ();

    if ($type_1 == $type_2) {
	if ($type_1 == XML_ELEMENT_NODE) {
	    return element_nodes_are_equal ($xml_node_1, $xml_node_2);
	} elsif ($type_1 == XML_ATTRIBUTE_NODE) {
	    return attribute_nodes_are_equal ($xml_node_1, $xml_node_2);
	} elsif ($type_1 == XML_TEXT_NODE) {
	    return text_nodes_are_equal ($xml_node_1, $xml_node_2);
	} else {
	    print {*STDERR} 'Error: unhandled node type ', $type_1, '.';
	    exit 1;
	}
    } else {
	return 0;
    }

}

my $verbose = 0;
my $debug = 0;
my $man = 0;
my $help = 0;
my $target_directory = undef;

GetOptions('help|?' => \$help,
	   'man' => \$man,
	   'verbose'  => \$verbose,
	   'debug' => \$debug)
    or pod2usage (2);

if ($help) {
    pod2usage (1);
}

if ($man) {
    pod2usage (-exitstatus => 0,
	       -verbose => 2);
}

if (scalar @ARGV < 2) {
    pod2usage (1);
}

my $xml_path_1 = $ARGV[0];
my $xml_path_2 = $ARGV[1];
my $xml_parser = XML::LibXML->new ();

if (-d $xml_path_1) {
    print {*STDERR} 'Error: ', $xml_path_1, ' is a directory, not a file.';
    exit 1;
}

if (-d $xml_path_2) {
    print {*STDERR} 'Error: ', $xml_path_2, ' is a directory, not a file.';
    exit 1;
}

if (! -e $xml_path_1) {
    print {*STDERR} 'Error: ', $xml_path_1, ' does not exist.';
}

if (! -e $xml_path_2) {
    print {*STDERR} 'Error: ', $xml_path_2, ' does not exist.';
}

if (! -r $xml_path_1) {
    print {*STDERR} 'Error: ', $xml_path_1, ' is unreadable.';
}

if (! -r $xml_path_2) {
    print {*STDERR} 'Error: ', $xml_path_2, ' is unreadable.';
}

my $xml_doc_1 = eval { $xml_parser->parse_file ($xml_path_1) };

if (! defined $xml_doc_1) {
    print {*STDERR} 'Error: ', $xml_path_1, ' is not a valid XML file.';
}

my $xml_doc_2 = eval { $xml_parser->parse_file ($xml_path_2) };

if (! defined $xml_doc_2) {
    print {*STDERR} 'Error: ', $xml_path_2, ' is not a valid XML file.';
}

my $xml_root_1 = $xml_doc_1->documentElement ();
my $xml_root_2 = $xml_doc_2->documentElement ();

exit (nodes_are_equal ($xml_root_1, $xml_root_2) ? 0 : 1);

__END__
