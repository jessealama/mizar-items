package Mizar;
use strict;
use File::Spec::Functions qw / catfile catdir /;

use base qw( Exporter );
our @EXPORT = qw(); # don't export anything by default
our @EXPORT_OK = qw( fetch_directive );
{

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

  my $workdir;
  sub get_workdir { defined $workdir ? $workdir : '.'; }
  sub set_workdir { 
    my $new_workdir = shift;
    unless (-d $new_workdir) {
      die "Proposed new work directory '$new_workdir' isn't actually a directory!";
    }
    $workdir = $new_workdir;
  }

  sub run_mizar_tool {
    my $tool = shift; # shift my tool
    my $article = shift;

    # verify
    my $tool_path = $mizar_tool_path{$tool};
    unless (defined $tool_path) {
      die "Unable to run mizar tool '$tool': we don't know where that is";
    }

    chdir get_workdir ();
    unless (system ("$tool_path $article > /dev/null 2> /dev/null") == 0) {
      die ("Something went wrong running the mizar tool '$tool' (under $tool_path): $!");
    }

    # check for a non-empty error file
    my $article_err = $article . '.err';
    unless (-z $article_err) {
      die "Although '$tool' exited successfully, it nonetheless generated a non-empty error file";
    }

    return;
  }

  sub verify {
    my $article = shift;
    run_mizar_tool ('verifier', $article);
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

}

1;
