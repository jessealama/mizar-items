package Mizar;

use warnings;
use strict;
use Carp;

use version; our $VERSION = qv('0.0.3');

use base qw(Exporter);
use IPC::Run qw(start);
use IPC::Cmd qw(can_run);
use Carp qw(croak);
use File::Basename qw(basename dirname);

use Utils qw(ensure_readable_file);

# Other recommended modules (uncomment to use):
#  use IO::Prompt;
#  use Perl6::Export;
#  use Perl6::Slurp;
#  use Perl6::Say;

our @EXPORT_OK = qw(accom
		    verifier
		    ensure_sensible_mizar_environment
		    path_for_stylesheet);

# Module implementation here

sub run_mizar_tool {

    my $tool = shift;
    my $article_name = shift;
    my $parameters_ref = shift;

    my %parameters = defined $parameters_ref ? %{$parameters_ref} : ();

    my $article_dirname = dirname ($article_name);
    my $article_basename = basename ($article_name, '.miz');
    my $article_err = "${article_dirname}/${article_basename}.err";

    my @tool_call = ($tool);

    my $long_lines = $parameters{'long-lines'};
    my $checker_only = $parameters{'checker-only'};
    my $quiet = $parameters{'quiet'};
    my $stop_on_error = $parameters{'stop-on-error'};

    if (defined $long_lines && $long_lines) {
	push (@tool_call, '-l');
    }

    if (defined $checker_only && $checker_only) {
	push (@tool_call, '-c');
    }

    if (defined $quiet && $quiet) {
	push (@tool_call, '-q');
    }

    if (defined $stop_on_error && $stop_on_error) {
	push (@tool_call, '-s');
    }

    push (@tool_call, $article_name);

    my $harness = start (\@tool_call,
			 '>', '/dev/null',
			 '2>', '/dev/null');

    $harness->finish ();
    my $exit_code = $harness->result (0);

    ($exit_code == 0) && (-r $article_err) && (-z $article_err) ? return 1 : return 0;

}

sub accom {
    my $article_name = shift;
    my $parameters_ref = shift;
    if (defined $parameters_ref) {
	return (run_mizar_tool ('accom', $article_name, $parameters_ref));
    } else {
	return (run_mizar_tool ('accom', $article_name));
    }
}

sub verifier {
    my $article_name = shift;
    my $parameters_ref = shift;
    if (not defined $article_name) {
	croak ('Error: bogus call to verifier.');
    }
    if (defined $parameters_ref) {
	return (run_mizar_tool ('verifier', $article_name, $parameters_ref));
    } else {
	return (run_mizar_tool ('verifier', $article_name));
    }
}

my %stylesheets = ();
my $stylesheet_home = "/Users/alama/sources/mizar/mizar-items/xsl";

sub set_stylesheet_home {
    my $new_home = shift;

    if (defined $new_home) {
	if (ensure_directory ($new_home)) {
	    my $old_home = $stylesheet_home;
	    $stylesheet_home = $new_home;
	    return $old_home;
	} else {
	    croak ('Error: cannot set ', $new_home, ' to be the new stylesheet home because that is not a directory.');
	}
    } else {
	croak ('Error: please supply a directory.');
    }

}

foreach my $sheet ('inferred-constructors',
		   'addabsrefs',
	           'properties-of-constructors',
	           'strip-property',
	           'properties-of-constructor',
	           'dependencies',
	           'split',
	           'itemize',
	           'wsm',
	           'extend-evl',
	           'conditions-and-properties',
	           'trim-properties-and-conditions',
	           'rewrite-aid') {
    my $stylesheet_path = "${stylesheet_home}/${sheet}.xsl";
    $stylesheets{$sheet} = $stylesheet_path;
}

sub path_for_stylesheet {
    my $sheet = shift;

    if (! defined $sheet) {
	croak ('Error: please supply the name of a stylesheet.');
    }

    my $path = $stylesheets{$sheet};

    if (defined $path) {
	if (ensure_readable_file ($path)) {
	    return $path;
	} else {
	    croak ('Error: the path for the ', $sheet, ' stylesheet is ', $path, ', but there is no file there (or the file is unreadable).');
	}
    } else {
	croak ('Error: the ', $sheet, ' stylesheet has no known path.');
    }
}

my @mml_lar = ();

my $mizfiles;

my @mizar_tools = ('verifier',
		   'accom',
		   'makeenv',
		   'exporter',
		   'transfer',
		   'msmprocessor',
		   'wsmparser',
		   'msplit',
		   'mglue');

sub ensure_sensible_mizar_environment {
  my $mizfiles = $ENV{'MIZFILES'};
  if (! defined $mizfiles) {
    croak ('Error: the MIZFILES environment variable is unset.');
  }
  if (! -e $mizfiles) {
    croak ('Error: the value of the MIZFILES environment variable, ', "\n", "\n", '  ', $mizfiles, "\n", "\n", 'does not exist.');
  }
  if (! -d $mizfiles) {
    croak ('Error: the value of the MIZFILES environment variable, ', "\n", "\n", '  ', $mizfiles, "\n", "\n", 'is not a directory.');
  }
  foreach my $tool (@mizar_tools) {
      if (! can_run ($tool)) {
	  croak ('Error: the needed Mizar tool ', $tool, ' seems to be unavailable.');
      }
  }
}

sub get_mizfiles { return $mizfiles; }

sub set_mizfiles {
    my ($new_mizfiles) = @_;

    # sanity
    unless (-e $new_mizfiles) {
	die "Cannot use the value '$new_mizfiles' as MIZFILES: no such file or directory";
    }
    unless (-d $new_mizfiles) {
	die "Cannot use the value '$new_mizfiles' as MIZFILES: not a directory";
    }

    # load mml.lar
    my $mml_lar_path = catfiles ($mizfiles, 'mml.lar');
    # first make sure it's available
    unless (-e $mml_lar_path) {
	die "Cannot use the value '$new_mizfiles' as MIZFILES: no file 'mml.lar' under the path";
    }
    unless (-r $mml_lar_path) {
	die "Cannot use the value '$new_mizfiles' as MIZFILES: the file 'mml.lar' is unreadable";
    }

    open my $mmllar, '<', $mml_lar_path
	or die "mml.lar cannot be opened: $!";
    while (<$mmllar>) {
	push (@mml_lar, $_);
    }
    close ($mmllar)
	or die "Can't close read-only filehandle for mml.lar: $!";

    chomp @mml_lar;
}

sub fetch_directive {
    my $directive = shift;
    my $article = shift;

    run_mizar_tool ('envget', $article);

    my $article_evl = $article . '.evl';

    # cheap approach: take advantage of the fact the the Directives in the
    # EVL file all begin at the beginning of the line
    my $evl_directive = "sed -n -e '/^<Directive name=\"$directive\"/,/^<\\/Directive/p' $article_evl";
    # another cheap trick like the one above
    my $select_identifiers = 'grep "^<Ident name="';

    # now delete all the padding
    my $name_equals_field = 'cut -f 2 -d \' \'';
    my $name_right_hand_side = 'cut -f 2 -d \'=\'';
    my $de_double_quote = 'sed -e \'s/"//g\'';

    my $big_pipe = "$evl_directive | $select_identifiers | $name_equals_field | $name_right_hand_side | $de_double_quote";

    my @directive_items = `$big_pipe`;
    chomp (@directive_items);
    @directive_items = grep (!/^HIDDEN$/, @directive_items);
    return @directive_items;
}

1; # Magic true value required at end of module
__END__

=pod

=head1 NAME

Mizar - [One line description of module's purpose here]


=head1 VERSION

This document describes Mizar version 0.0.1


=head1 SYNOPSIS

    use Mizar;

=for author to fill in:
    Brief code example(s) here showing commonest usage(s).
    This section will be as far as many users bother reading
    so make it as educational and exeplary as possible.


=head1 DESCRIPTION

=for author to fill in:
    Write a full description of the module and its features here.
    Use subsections (=head2, =head3) as appropriate.


=head1 INTERFACE

=for author to fill in:
    Write a separate section listing the public components of the modules
    interface. These normally consist of either subroutines that may be
    exported, or methods that may be called on objects belonging to the
    classes provided by the module.

=over

=item verify

=back

Run the Mizar verifier.


=head1 DIAGNOSTICS

=for author to fill in:
    List every single error and warning message that the module can
    generate (even the ones that will "never happen"), with a full
    explanation of each problem, one or more likely causes, and any
    suggested remedies.

=over

=item C<< Error message here, perhaps with %s placeholders >>

[Description of error here]

=item C<< Another error message here >>

[Description of error here]

[Et cetera, et cetera]

=back


=head1 CONFIGURATION AND ENVIRONMENT

=for author to fill in:
    A full explanation of any configuration system(s) used by the
    module, including the names and locations of any configuration
    files, and the meaning of any environment variables or properties
    that can be set. These descriptions must also include details of any
    configuration language used.

Mizar requires no configuration files or environment variables.


=head1 DEPENDENCIES

=for author to fill in:
    A list of all the other modules that this module relies upon,
    including any restrictions on versions, and an indication whether
    the module is part of the standard Perl distribution, part of the
    module's distribution, or must be installed separately. ]

None.


=head1 INCOMPATIBILITIES

=for author to fill in:
    A list of any modules that this module cannot be used in conjunction
    with. This may be due to name conflicts in the interface, or
    competition for system or program resources, or due to internal
    limitations of Perl (for example, many modules that use source code
    filters are mutually incompatible).

None reported.


=head1 BUGS AND LIMITATIONS

=for author to fill in:
    A list of known problems with the module, together with some
    indication Whether they are likely to be fixed in an upcoming
    release. Also a list of restrictions on the features the module
    does provide: data types that cannot be handled, performance issues
    and the circumstances in which they may arise, practical
    limitations on the size of data sets, special cases that are not
    (yet) handled, etc.

No bugs have been reported.

Please report any bugs or feature requests to
C<bug-mizar@rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org>.


=head1 AUTHOR

Jesse Alama  C<< <jesse.alama@gmail.com> >>


=head1 LICENCE AND COPYRIGHT

Copyright (c) 2012, Jesse Alama C<< <jesse.alama@gmail.com> >>. All rights reserved.

This module is free software; you can redistribute it and/or
modify it under the same terms as Perl itself. See L<perlartistic>.


=head1 DISCLAIMER OF WARRANTY

BECAUSE THIS SOFTWARE IS LICENSED FREE OF CHARGE, THERE IS NO WARRANTY
FOR THE SOFTWARE, TO THE EXTENT PERMITTED BY APPLICABLE LAW. EXCEPT WHEN
OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR OTHER PARTIES
PROVIDE THE SOFTWARE "AS IS" WITHOUT WARRANTY OF ANY KIND, EITHER
EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE
ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE SOFTWARE IS WITH
YOU. SHOULD THE SOFTWARE PROVE DEFECTIVE, YOU ASSUME THE COST OF ALL
NECESSARY SERVICING, REPAIR, OR CORRECTION.

IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING
WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY AND/OR
REDISTRIBUTE THE SOFTWARE AS PERMITTED BY THE ABOVE LICENCE, BE
LIABLE TO YOU FOR DAMAGES, INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL,
OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OR INABILITY TO USE
THE SOFTWARE (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA BEING
RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD PARTIES OR A
FAILURE OF THE SOFTWARE TO OPERATE WITH ANY OTHER SOFTWARE), EVEN IF
SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF
SUCH DAMAGES.

=cut
