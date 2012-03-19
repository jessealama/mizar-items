#!/usr/bin/perl

use strict;
use warnings;

use Getopt::Long qw(:config gnu_compat);
use Pod::Usage;
use Readonly;
use charnames qw(:full);
use English qw(-no_match_vars);
use version;
use Carp qw(croak carp);
use Term::ANSIColor;
use Regexp::DefaultFlags;
use List::MoreUtils qw(all);
use Cwd;

use FindBin qw($RealBin);
use lib "$RealBin/../lib";

use Utils qw(ensure_directory);
use ItemizedArticle;

use version;
Readonly my $VERSION => qv('1.0');

# Constant strings
Readonly my $EMPTY_STRING => q{};
Readonly my $SPACE => q{ };
Readonly my $TWO_SPACES => q{  };

Readonly my @COMMANDS =>
    (
	'cat-fragment',
	'dependencies',
	'items-to-fragments',
	'fragments-to-items',
    );

Readonly my %COMMAND_ROUTINES =>
    (
	'cat-fragment' => \&cmd_cat_fragment,
	'dependencies' => \&cmd_dependencies,
	'items-to-fragments' => \&cmd_items_to_fragments,
	'fragments-to-items' => \&cmd_fragment_to_items,
    );
Readonly my %COMMAND_DESCRIPTIONS =>
    (
	'cat-fragment'   => 'Show the Mizar text of a fragment.',
	'dependencies' => 'Show the dependencies of an item or itemized article.',
	'items-to-fragments' => 'Show the mapping of items to fragments.',
	'fragments-to-items' => 'Show the mapping of fragments to items.',
    );

my $opt_man       = 0;
my $opt_help      = 0;
my $opt_verbose   = 0;
my $opt_debug     = 0;
my $opt_version   = 0;
my $opt_dir       = undef;

my $items_dir = undef;
my $script_home = "$RealBin/../../bin";
my $stylesheet_home = "$RealBin/../../xsl";

sub describe_command {
    my $command_name = shift;
    if (defined $COMMAND_DESCRIPTIONS{$command_name}) {
	return $COMMAND_DESCRIPTIONS{$command_name};
    } else {
	print {*STDERR} message (error_message ('No description is known for the command \'', $command_name, '\'.'));
	exit 1;
    }
}

sub ensure_sensible_commands {

    my $all_descriptions_known = all { defined $COMMAND_DESCRIPTIONS{$_} } @COMMANDS;
    my $all_subroutines_defined = all { defined $COMMAND_ROUTINES{$_} } @COMMANDS;

    return ($all_descriptions_known && $all_subroutines_defined);

}

sub message {
    my @message_parts = @_;
    my $msg = join $EMPTY_STRING, @message_parts;
    return $msg . "\N{LF}";
}

sub message_with_extra_linefeed {
    my @message_parts = @_;
    my $msg = join $EMPTY_STRING, @message_parts;
    return message (message ($msg));
}

sub error_message {
    my @message_parts = @_;
    my $message = join ($EMPTY_STRING, @message_parts);
    my $message_with_error_padding = colored ('Error', 'red') . ': ' . $message;
    return $message_with_error_padding;
}

sub summarize_commands {

    my $summary = $EMPTY_STRING;

    foreach my $command (@COMMANDS) {
	my $description = describe_command ($command);
	$summary .= message ('  * ', colored ($command, 'blue'), ' -- ', $description);
    }

    return $summary;

}

sub process_commandline {

    GetOptions(
        'help|?'      => \$opt_help,
        'man'         => \$opt_man,
        'verbose'     => \$opt_verbose,
        'debug'       => \$opt_debug,
        'version'     => \$opt_version,
	'dir=s'       => \$opt_dir,
    ) or pod2usage(2);

    if ($opt_help) {
        pod2usage(1);
    }

    if ($opt_man) {
        pod2usage(
            -exitstatus => 0,
            -verbose    => 2
        );
    }

    if ($opt_version) {
        print message ($VERSION);
        exit 0;
    }

    # debug implies verbose
    if ($opt_debug) {
        $opt_verbose = 1;
    }

    if (scalar @ARGV == 0) {
	my $message = message_with_extra_linefeed ('Please supply a mizar-items command.  The available commands are:');
	$message .= message (summarize_commands ());
	$message .= message ('See the man page for more information.  (Invoke this program with the option \'--man\'.)');
	pod2usage (-message => error_message ($message),
		   -exitstatus => 2);
    }

    my $command = $ARGV[0];

    if (! defined $COMMAND_ROUTINES{$command}) {
	my $message = message_with_extra_linefeed ('Unknown command');
	$message .= message_with_extra_linefeed ($TWO_SPACES, $command);
	$message .= message_with_extra_linefeed ('Supported commands:');
	$message .= message (summarize_commands ());
	$message .= message ('See the man page for more details.  (Invoke this program with the option \'--man\'.)');
	pod2usage (-message => error_message ($message),
		   -exitstatus => 2);

    }

    if (defined $opt_dir) {
	if (ensure_directory ($opt_dir)) {
	    $items_dir = $opt_dir;
	} else {
	    print {*STDERR} error_message ('The given directory ', $opt_dir, ' is not a directory.');
	    exit 1;
	}
    } else {
	$items_dir = getcwd ();
    }

    if ( ! ItemizedArticle::ensure_appropriate_directory_structure ($items_dir) ) {
	print {*STDERR} message (error_message ('The given directory', "\N{LF}", "\N{LF}", $TWO_SPACES, $items_dir, "\N{LF}", "\N{LF}", 'does not have the expected itemized article directory structure.'));
	exit 1;
    }

    return 1;

}

if (! ensure_sensible_commands ()) {
    print {*STDERR} error_message ('Something is badly wrong with the list of commands; please inform the maintainer.'), "\N{LF}";
    exit 1;
}

process_commandline ();

my $command_name = $ARGV[0];
shift @ARGV;

my $eval_command = eval { &{$COMMAND_ROUTINES{$command_name}} };
my $eval_message = $@;

if (defined $eval_command) {
    exit 0;
} elsif (defined $eval_message) {
    print {*STDERR} error_message ('Something went wrong executing the command \'', $command_name, '\'');
    if (scalar @ARGV == 0) {
	print {*STDERR} message (' (without any further arguments).');
    } else {
	my $argument_list = join ($SPACE, @ARGV);
	print {*STDERR} message (' with the arguments', "\N{LF}");
	print {*STDERR} message ($TWO_SPACES, $argument_list, "\N{LF}");
    }
    print {*STDERR} message ('The error was:', "\N{LF}");
    print {*STDERR} message ($eval_message);
    print {*STDERR} message ('Please inform the maintainers.');
    exit 1;
} else {
    print {*STDERR} error_message ('Something went badly wrong trying to execute the command \'', $command_name, '\'.');
    print {*STDERR} message ('Please inform the maintainers.');
    exit 1;
}

######################################################################
## cat-fragment
######################################################################

sub ensure_sensible_cat_fragment_arguments {

    if (scalar @ARGV == 0) {
	print {*STDERR} message (error_message ('The mandatory argument to cat-fragment is missing.'));
	exit 1;
    }

    if (scalar @ARGV > 1) {
	print {*STDERR} message (error_message ('cat-fragment expects exactly one argument.'));
	exit 1;
    }

    return 1;

}

sub cmd_cat_fragment {

    ensure_sensible_cat_fragment_arguments ();

    my $fragment = $ARGV[0];

    my $itemized_article = eval { ItemizedArticle->new (location => $items_dir,
						        script_home => $script_home,
						        stylesheet_home => $stylesheet_home) };
    my $itemized_article_message = $@;

    if (! defined $itemized_article) {
	if (defined $itemized_article_message) {
	    print {*STDERR} message (error_message ('We failed to make an ItemizedArticle object.'));
	    print {*STDERR} message_with_extra_linefeed ('The error message was:');
	    print {*STDERR} message ($itemized_article_message);
	    exit 1;
	} else {
	    print {*STDERR} message (error_message ('We failed to make an ItemizedArticle object for ', $items_dir, '.'));
	    print {*STDERR} message ('We also (somehow) did not even manage to get an error message, so nothing further remains to be said.');
	    exit 1;
	}
    }

    my $fragment_text = eval {$itemized_article->text_for_fragment ($fragment) };
    my $fragment_text_message = $@;

    if (defined $fragment_text) {
	print $fragment_text;
    } elsif (defined $fragment_text_message) {
	print {*STDERR} message_with_extra_linefeed (error_message ('Something went wrong when extracting the text for fragment ', $fragment, ' in the itemized article directory ', $items_dir, ':'));
	print {*STDERR} message ($fragment_text_message);
	exit 1;
    } else {
	print {*STDERR} message (error_message ('Something went wrong when extracting the text for fragment ', $fragment, ' in the itemized article directory ', $items_dir, '.'));
	print {*STDERR} message ('We (somehow) failed to even get an error message about this, so no further information is available.  Sorry.');
	exit 1;
    }

}
__END__
