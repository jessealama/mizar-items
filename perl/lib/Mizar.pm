package Mizar;

use warnings;
use strict;
use Carp;

use version; our $VERSION = qv('0.0.3');

use base qw(Exporter);
use IPC::Run qw(start);
use Carp qw(croak);
use File::Basename qw(basename dirname);

# Other recommended modules (uncomment to use):
#  use IO::Prompt;
#  use Perl6::Export;
#  use Perl6::Slurp;
#  use Perl6::Say;

our @EXPORT_OK = qw(verify);

# Module implementation here

sub verify {
    my $article_name = shift;
    my $parameters_ref = shift;

    my %parameters = defined $parameters_ref ? %{$parameters_ref} : ();

    my $article_dirname = dirname ($article_name);
    my $article_basename = basename ($article_name, '.miz');
    my $article_err = "${article_dirname}/${article_basename}.err";

    my @verifier_call = ('verifier');

    my $long_lines = $parameters{'long-lines'};
    my $checker_only = $parameters{'checker-only'};
    my $quiet = $parameters{'quiet'};
    my $stop_on_error = $parameters{'stop-on-error'};

    if (defined $long_lines && $long_lines) {
	push (@verifier_call, '-l');
    }

    if (defined $checker_only && $checker_only) {
	push (@verifier_call, '-c');
    }

    if (defined $quiet && $quiet) {
	push (@verifier_call, '-q');
    }

    if (defined $stop_on_error && $stop_on_error) {
	push (@verifier_call, '-s');
    }

    push (@verifier_call, $article_name);

    my $harness = start (\@verifier_call,
			 '>', '/dev/null',
			 '2>', '/dev/null');

    $harness->finish ();
    my $exit_code = $harness->result (0);

    ($exit_code == 0) && (-r $article_err) && (-z $article_err) ? return 1 : return 0;

}

my @mml_lar = ();

my $mizfiles;

sub which_chomp {
    my $thing = shift;
    my $which = `which $thing`;
    chomp $which;
    return $which;
}

# mizar tools
my %mizar_tool_path
    = ('makeenv' => which_chomp ('makeenv'),
       'accom' => which_chomp ('accom'),
       'verifier' => which_chomp ('verifier'),
       'dellink' => which_chomp ('dellink'),
       'JA1' => which_chomp ('JA1'));
my @mizar_tools = keys %mizar_tool_path;

# verify that these are all executable
foreach my $tool (@mizar_tools) {
    my $tool_path = $mizar_tool_path{$tool};
    unless (-e $tool_path) {
	die "The path '$tool_path' for the mizar tool '$tool' doesn't exist";
    }
    if (-d $tool_path) {
	die "The path '$tool_path' for the mizar tool '$tool' is actually a directory!";
    }
    unless (-x $tool_path) {
	die "The path '$tool_path' for the mizar tool '$tool' is not executable";
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