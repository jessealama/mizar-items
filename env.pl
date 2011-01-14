#!/usr/bin/perl -w

# quickly extract the content of an environment directive for an
# article.
#
# This kind of tool has probably been written a billion times.  Oh well.

sub usage {
  print ("Usage: list-vocab.pl DIRECTIVE ARTICLE\n");
  print ("\n");
  print ("DIRECTIVE must be one of\n");
  print ("\n");
  print ("  Vocabularies\n");
  print ("  Notations\n");
  print ("  Constructors\n");
  print ("  Registrations\n");
  print ("  Requirements\n");
  print ("  Definitions\n");
  print ("  Theorems\n");
  print ("  Schemes\n");
  print ("\n");
}

if ($#ARGV == -1 || $#ARGV == 0 || $#ARGV > 2) { # huh? why -1?
  usage ();
  exit (-1);
}

# DEBUG
# warn ("number of arguments: $#ARGV\n");

my $directive = $ARGV[0];

# DEBUG
# warn ("Directive is: $directive\n");

unless ($directive =~ '^Vocabularies$|^Notations$|^Constructors$|^Registrations$|^Requirements$|^Definitions$|^Theorems$|^Schemes$') {
  usage ();
  exit (-1);
}

my $article_base = $ARGV[1];
my $article_miz = $article_base . '.miz';
my $article_evl = $article_base . '.evl';

system ("envget -l $article_base > /dev/null 2> /dev/null");
# unless ($? == 0) {
#   die ("Something went wrong when calling envget on $article_base.\nThe error was\n\n  $!");
# }

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

# DEBUG
# warn ("about execute this big guy:\n\n  $big_pipe");

my @directive_items = `$big_pipe`;
if ($? == 0) {
  foreach my $item (@directive_items) {
    print ($item);
  }
} else {
  die ("Something went wrong executing the command\n\n$big_pipe\n\nThe error was: $!");
}

exit (0);
