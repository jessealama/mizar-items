use Test::More qw(no_plan);

use warnings;
use strict;

# Set up our dummy article
BEGIN {
  use_ok ('Readonly');
}

BEGIN {
    use_ok ('File::Temp', qw(tempdir));
}

BEGIN {
    use_ok ('Utils', qw(write_string_to_file));
}

BEGIN {
    use_ok ('Article');
}

BEGIN {
    use_ok ('LocalDatabase');
}

Readonly my $empty => <<'END_ARTICLE';
environ begin
END_ARTICLE

Readonly my $one_proposition_lemma => <<'END_ARTICLE';
environ begin
for x being set holds x = x;
END_ARTICLE

Readonly my $two_proposition_lemmas => <<'END_ARTICLE';
environ begin
for x being set holds x = x;
for x being set holds x = x;
END_ARTICLE

Readonly my $one_diffuse_lemma => <<'END_ARTICLE';
environ begin
now let x be set; thus x = x; end;
END_ARTICLE

Readonly my $one_iter_equality => <<'END_ARTICLE';
environ begin
the set = the set .= the set;
END_ARTICLE

Readonly my $one_consider => <<'END_ARTICLE';
environ begin
the set = the set;
then consider x being set such that x = x;
END_ARTICLE

Readonly my $two_considers => <<'END_ARTICLE';
environ begin
the set = the set;
then consider x, y being set such that x = x and y = y;
END_ARTICLE

# Save all this to disk

my $dir = tempdir ();

######################################################################
## empty article
######################################################################
my $a0_path = "${dir}/a0.miz";

write_string_to_file ($empty, $a0_path);

my $a0 = Article->new (path => $a0_path);
my $a0_itemized = $a0->itemize ("${dir}/a0");

is (scalar @{$a0_itemized->get_all_items ()}, 0,
    'a0 has zero items');

diag ('items of a0: ', @{$a0_itemized->get_all_items ()});

my @a0_lemmas = $a0_itemized->get_lemmas ();

is (scalar @a0_lemmas, 0,
    'a0 has zero lemmas');

######################################################################
## one lemma
######################################################################
my $a1_path = "${dir}/a1.miz";

write_string_to_file ($one_proposition_lemma, $a1_path);

my $a1 = Article->new (path => $a1_path);
my $a1_itemized = $a1->itemize ("${dir}/a1");

is (scalar @{$a1_itemized->get_all_items ()}, 1,
    'a1 has exactly 1 item');

diag ('items of a1: ', @{$a1_itemized->get_all_items ()});

my @a1_lemmas = $a1_itemized->get_lemmas ();

is (scalar @a1_lemmas, 1,
    'a1 has exactly one lemma');

######################################################################
## one diffuse lemma
######################################################################
my $a2_path = "${dir}/a2.miz";

write_string_to_file ($one_diffuse_lemma, $a2_path);

my $a2 = Article->new (path => $a2_path);
my $a2_itemized = $a2->itemize ("${dir}/a2");

is (scalar @{$a2_itemized->get_all_items ()}, 1,
    'a2 has exactly 1 item');

diag ('items of a2: ', @{$a2_itemized->get_all_items ()});

my @a2_lemmas = $a2_itemized->get_lemmas ();

is (scalar @a2_lemmas, 1,
    'a2 has exactly one lemma');

######################################################################
## one iter equality
######################################################################
my $a3_path = "${dir}/a3.miz";

write_string_to_file ($one_iter_equality, $a3_path);

my $a3 = Article->new (path => $a3_path);
my $a3_itemized = $a3->itemize ("${dir}/a3");

is (scalar @{$a3_itemized->get_all_items ()}, 1,
    'a3 has exactly 1 item');

diag ('items of a3: ', @{$a3_itemized->get_all_items ()});

my @a3_lemmas = $a3_itemized->get_lemmas ();

is (scalar @a3_lemmas, 1,
    'a3 has exactly one lemma');

######################################################################
## a proposition and a proposition inside a consider
######################################################################
my $a4_path = "${dir}/a4.miz";

write_string_to_file ($one_consider, $a4_path);

my $a4 = Article->new (path => $a4_path);
my $a4_itemized = $a4->itemize ("${dir}/a4");

is (scalar @{$a4_itemized->get_all_items ()}, 2,
    'a4 has exactly 2 items');

diag ('items of a4: ', @{$a4_itemized->get_all_items ()});

my @a4_lemmas = $a4_itemized->get_lemmas ();

is (scalar @a4_lemmas, 2,
    'a4 has exactly two lemmas');

######################################################################
## two consider proposition
######################################################################
my $a5_path = "${dir}/a5.miz";

write_string_to_file ($two_considers, $a5_path);

my $a5 = Article->new (path => $a5_path);
my $a5_itemized = $a5->itemize ("${dir}/a5");

is (scalar @{$a5_itemized->get_all_items ()}, 3,
    'a5 has exactly 3 items');

diag ('items of a5: ', @{$a5_itemized->get_all_items ()});

my @a5_lemmas = $a5_itemized->get_lemmas ();

is (scalar @a5_lemmas, 3,
    'a5 has exactly three lemmas');


### Local Variables: ***
### mode:perl ***
### End: ***
