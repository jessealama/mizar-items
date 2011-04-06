#!/usr/bin/perl -w

use strict;

my $itemization_source = '/Users/alama/sources/mizar/mizar-items/itemization';
my $mml_lar = '/sw/share/mizar/mml.lar';
my $items_needed_script = '/Users/alama/sources/mizar/mizar-items/items-needed-for-item.sh';

sub nr_attribute {
  my $xml_line = shift;
  $xml_line =~ / nr=\"([0-9]+)\"/;
  return $1;
}

sub constrnr_attribute {
  my $xml_line = shift;
  $xml_line =~ / constrnr=\"([0-9]+)\"/;
  return $1;
}

sub defnr_attribute {
  my $xml_line = shift;
  $xml_line =~ / defnr=\"([0-9]+)\"/;
  return $1;
}

sub absconstrnr_attribute {
  my $xml_line = shift;
  $xml_line =~ / absconstrnr=\"([0-9]+)\"/;
  return $1;
}

sub aid_attribute {
  my $xml_line = shift;
  $xml_line =~ / aid=\"(\w+)\"/;
  return $1;
}

sub schemenr_attribute {
  my $xml_line = shift;
  $xml_line =~ / schemenr=\"([0-9]+)\"/;
  return $1;
}

sub kind_attribute {
  my $xml_line = shift;
  $xml_line =~ / kind=\"(\w)\"/;
  return $1;
}

sub constrkind_attribute {
  my $xml_line = shift;
  $xml_line =~ / constrkind=\"(\w)\"/;
  return $1;
}

my $num_articles_handled = 200;
# my @articles = ('tarski');
my @articles = ();
my @first_hundred = `head -n $num_articles_handled $mml_lar`;
chomp @first_hundred;
push (@articles, @first_hundred);


# keys are strings such as "xboole_0:deftheorem:1", values are string
# like "xboole_0:1", which means: xboole_0 item 1 gives rise to
# xboole_0 deftheorem #1.

# 'set'
my %mml_name_to_item_number 
  = (
     # hard-coded hidden mappings

     # set
     'hidden:mpattern:1' => 'hidden:1',
     'hidden:mconstructor:1' => 'hidden:1',

     # =
     'hidden:rconstructor:1' => 'hidden:2',
     'hidden:rpattern:1' => 'hidden:2',

     # <>
     'hidden:rpattern:2' => 'hidden:3',

     # in
     'hidden:rconstructor:2' => 'hidden:4',
     'hidden:rpattern:3' => 'hidden:4',

     # hard-coded tarski mappings

     # 1: canceled
     #
     # canceled;
     'tarski:theorem:1' => 'tarski:1',

     # 2: extensionality
     #
     # theorem
     # (for x holds x in X iff x in Y) implies X = Y;
     'tarski:theorem:2' => 'tarski:2',

     # 3: singleton
     #
     # definition
     #   let y;
     #   func
     #     { y } -> set
     #   means
     #     x in it iff x = y;
     #   correctness;
     # end;
     'tarski:kconstructor:1' => 'tarski:3',
     'tarski:definiens:1' => 'tarski:3',
     'tarski:kpattern:1' => 'tarski:3',
     'tarski:deftheorem:1' => 'tarski:3',

     # 4: unordered pair
     #
     # definition
     #   let y, z;
     #   func
     #     { y, z } -> set
     #   means
     #     x in it iff x = y or x = z;
     #   correctness;
     #   commutativity;
     # end;
     'tarski:kconstructor:2' => 'tarski:4',
     'tarski:kpattern:2' => 'tarski:4',
     'tarski:definiens:2' => 'tarski:4',
     'tarski:deftheorem:2' => 'tarski:4',

     # 5: canceled
     'tarski:theorem:3' => 'tarski:5',

     # 6: canceled
     'tarski:theorem:4' => 'tarski:6',

     # 7: definition of subset relation
     #
     # definition
     #   let X,Y;
     #   pred
     #     X c= Y
     #   means
     #     x in X implies x in Y;
     # end;
     'tarski:rconstructor:1' => 'tarski:7',
     'tarski:definiens:3' => 'tarski:7',
     'tarski:rpattern:1' => 'tarski:7',
     'tarski:deftheorem:3' => 'tarski:7',

     # 8: union
     #
     # definition
     #   let X;
     #   func
     #     union X -> set
     #   means
     #     x in it iff ex Y st x in Y & Y in X;
     #   correctness;
     # end;
     'tarski:kconstructor:3' => 'tarski:8',
     'tarski:definiens:4' => 'tarski:8',
     'tarski:kpattern:3' => 'tarski:8',
     'tarski:deftheorem:4' => 'tarski:8',

     # 9: canceled
     'tarski:theorem:5' => 'tarski:9',

     # 10: canceled
     'tarski:theorem:6' => 'tarski:10',

     # 11: foundation
     #
     # theorem
     # x in X implies ex Y st Y in X & not ex x st x in X & x in Y;
     'tarski:theorem:7' => 'tarski:11',

     # 12: replacement
     #
     # scheme Fraenkel { A()-> set, P[set, set] } :
     #  ex X st for x holds x in X iff ex y st y in A() & P[y,x]
     # provided
     #  for x,y,z st P[x,y] & P[x,z] holds y = z
     # proof
     #   thus thesis;
     # end;
     'tarski:scheme:1' => 'tarski:12',

     # 13: definition of ordered pair
     #
     # definition
     #   let x,y;
     #   func
     #     [x,y]
     #   means
     #     it = { { x,y }, { x } };
     #   correctness;
     # end;
     'tarski:kconstructor:4' => 'tarski:13',
     'tarski:definiens:5' => 'tarski:13',
     'tarski:kpattern:4' => 'tarski:13',
     'tarski:deftheorem:5' => 'tarski:13',

     # 14: canceled
     'tarski:theorem:8' => 'tarski:13',

     # 15: definition of equipotent sets
     # definition
     #   let X,Y;
     #   pred
     #     X,Y are_equipotent
     #   means
     #     ex Z st
     #       (for x st x in X ex y st y in Y & [x,y] in Z) &
     #       (for y st y in Y ex x st x in X & [x,y] in Z) &
     #       for x,y,z,u st [x,y] in Z & [z,u] in Z holds x = z iff y = u;
     # end;
     'tarski:rconstructor:2' => 'tarski:15',
     'tarski:definiens:6' => 'tarski:15',
     'tarski:rpattern:2' => 'tarski:15',
     'tarski:deftheorem:6' => 'tarski:15',

     # 16: Tarski universe axiom
     #
     # theorem
     #   ex M st N in M &
     #      (for X,Y holds X in M & Y c= X implies Y in M) &
     #      (for X st X in M ex Z st Z in M & for Y st Y c= X holds Y in Z) &
     #      (for X holds X c= M implies X,M are_equipotent or X in M);
     'tarski:theorem:9' => 'tarski:16',
     );

# my %hidden_items 
#   = ('set:pattern' => 'hidden:mpattern:1',
#      'set:constructor' => 'hidden:mconstructor:1',
#      '=:constructor' => 'hidden:rconstructor:1',
#      '=:pattern' => 'hidden:rpattern:1',
#      '<>:pattern' => 'hidden:rpattern:2',
#      'in:constructor' => 'hidden:rconstructor:2',
#      'in:pattern' => 'hidden:rpattern:3');

my %hidden_items
  = ('set:pattern' => 'hidden:1',
     'set:constructor' => 'hidden:1',
     '=:constructor' => 'hidden:2',
     '=:pattern' => 'hidden:2',
     '<>:pattern' => 'hidden:3',
     'in:constructor' => 'hidden:4',
     'in:pattern' => 'hidden:4');

{
  my %num_propositions_for_article = ();

  sub register_item_for_article {
    # We are given as input an XML line like
    #
    # :: <Proposition propnr="2" plevel="" vid="30" nr="2" col="22" line="41">
    #
    # which is the second, commented-out line of a microarticle
    #
    # The first argument is an article name, such as "tarski" or
    # "valued_0", and the second argument is a number, representing
    # the item number of the article.
    my $article_name = shift;
    my $item_num = shift;
    my $xml_fragment = shift;

    chdir "$itemization_source/$article_name";
    my $item_miz = "text/ckb$item_num.miz";
    unless (-e $item_miz) {
	die "Can't find '$item_miz' here!"
    }
    my $second_line = `head -n 2 $item_miz | tail -n 1`;
    chomp $second_line;

    # warn "looking at second line $second_line";

    # special case: we need to keep track of toplevel unexported
    # propositions
    if ($second_line =~ /Proposition/) {
      my $num_props_for_article = $num_propositions_for_article{$article_name};
      if (defined $num_props_for_article) {
	$num_propositions_for_article{$article_name}
	  = $num_props_for_article + 1;
      } else {
	$num_propositions_for_article{$article_name} = 1;
      }
    }

    if ($second_line =~ /Proposition/) {
      my $prop_nr = $num_propositions_for_article{$article_name};
      $mml_name_to_item_number{"$article_name:lemma:$prop_nr"}
	= "$article_name:$item_num";
    } elsif ($second_line =~ /JustifiedTheorem/) {
      my $nr = nr_attribute ($second_line);
	$mml_name_to_item_number{"$article_name:theorem:$nr"}
	  = "$article_name:$item_num";
    } elsif ($second_line =~ /DefinitionBlock/) {

      # find any constructor, pattern, definiens and deftheorem

      my @constructor_lines = `grep '<Constructor ' $item_miz`;
      foreach my $constructor_line (@constructor_lines) {
	chomp $constructor_line;
	unless ($constructor_line eq '') {
	  my $constructor_nr = nr_attribute ($constructor_line);
	  my $constructor_kind = kind_attribute ($constructor_line);
	  my $constructor_kind_lc = lc $constructor_kind;
	  my $key = "$article_name" . ":" . "$constructor_kind_lc" . "constructor" . ":" . $constructor_nr;
	  $mml_name_to_item_number{$key} = "$article_name:$item_num";
	}
      }

      my @pattern_lines = `grep '<Pattern ' $item_miz`;
      foreach my $pattern_line (@pattern_lines) {
	chomp $pattern_line;
	unless ($pattern_line eq '') {
	  my $pattern_nr = nr_attribute ($pattern_line);
	  my $pattern_kind = kind_attribute ($pattern_line);
	  my $pattern_kind_lc = lc $pattern_kind;
	  my $key = "$article_name" . ":" . "$pattern_kind_lc" . "pattern" . ":" . "$pattern_nr";
	  $mml_name_to_item_number{$key} = "$article_name:$item_num";
	}
      }

      my $definiens_line = `grep --max-count=1 '<Definiens ' $item_miz`;
      chomp $definiens_line;
      unless ($definiens_line eq '') {
	  my $defnr = defnr_attribute ($definiens_line);
	  my $constrkind = constrkind_attribute ($definiens_line);
	  my $constrkind_lc = lc $constrkind;
	  my $key = "$article_name" . ":" . "definiens" . ":" . "$defnr";
	  $mml_name_to_item_number{$key} = "$article_name:$item_num";
      }

      my $deftheorem_line = `grep --max-count=1 '<DefTheorem ' $item_miz`;
      chomp $deftheorem_line;
      unless ($deftheorem_line eq '') {
	my $deftheorem_nr = nr_attribute ($deftheorem_line);
	my $key = "$article_name:deftheorem:$deftheorem_nr";
	$mml_name_to_item_number{$key} = "$article_name:$item_num";
      }

    } elsif ($second_line =~ /SchemeBlock/) {
      my $schemenr = schemenr_attribute ($second_line);
      $mml_name_to_item_number{"$article_name:scheme:$schemenr"}
	= "$article_name:$item_num";
    } elsif ($second_line =~ /RegistrationBlock/) {
      my $cluster_line = `grep --max-count=1 '<[RCF]Cluster ' $item_miz`;
      my $identify_line = `grep --max-count=1 '<Identify ' $item_miz`;
      chomp $cluster_line;
      chomp $identify_line;
      if ($identify_line eq '') {
	my $cluster_kind;
	if ($cluster_line =~ /RCluster/) {
	  $cluster_kind = 'rcluster';
	} elsif ($cluster_line =~ /CCluster/) {
	  $cluster_kind = 'ccluster';
	} else {
	  $cluster_kind = 'fcluster'
	}
	my $cluster_nr = nr_attribute ($cluster_line);
	$mml_name_to_item_number{"$article_name:$cluster_kind:$cluster_nr"}
	  = "$article_name:$item_num";
      } else {
	my $constrkind = constrkind_attribute ($identify_line);
	my $constrkind_lc = lc $constrkind;
	my $nr = nr_attribute ($identify_line);
	my $key = "$article_name" . ":" . $constrkind_lc . "identification" . ":" . $nr;
	$mml_name_to_item_number{$key} = "$article_name:$item_num";
      }
    } elsif ($second_line =~ /NotationBlock/) {
      # BAD: I don't currently split notation blocks, so grabbing only
      # the first pattern is, in general, not correct because some
      # patterns might be ignored
      my @pattern_lines = `grep --only-matching '<Pattern .*' $item_miz`;
      foreach my $pattern_line (@pattern_lines) {
	chomp $pattern_line;
	my $pattern_nr = nr_attribute ($pattern_line);
	my $pattern_kind = kind_attribute ($pattern_line);
	my $pattern_kind_lc = lc $pattern_kind;
	my $key = $article_name . ":" . $pattern_kind_lc . "pattern" . ":" . $pattern_nr;
	$mml_name_to_item_number{$key}
	  = "$article_name:$item_num";
      }
    } else {
      warn "Don't know how to handle an item that looks like '$second_line', coming from $item_miz";
    }
  }
}

my $ckb_ckb_depgraph = "/Users/alama/sources/mizar/mizar-items/ckb-ckb-depgraph";
my $mizar_item_to_ckb = "/Users/alama/sources/mizar/mizar-items/mizar-item-ckb-table";

# don't overwrite -- these computations are hard-earned!
if (-e $ckb_ckb_depgraph) {
  die "There is already a dependency graph at '$ckb_ckb_depgraph' -- refusing to overwrite it.";
}
if (-e $mizar_item_to_ckb) {
  die "There is already a mizar-item-to-CKB table at '$mizar_item_to_ckb' -- refusing to overwrite it.";
}

# build our table
foreach my $article (@articles) {
  chdir "$itemization_source/$article";
  my @items = `find text -name "ckb*.miz"`;
  chomp @items;
  foreach my $item_num (1 .. scalar @items) {
    my $item = $items[$item_num - 1];
    # warn "about to register item $item of article $article...";
    register_item_for_article ($article, $item_num);
  }
}

open (DEPGRAPH, '>', $ckb_ckb_depgraph)
  or die "Couldn't open an output filehandle for '$ckb_ckb_depgraph': $!";

# now compute the dependencies
foreach my $article (@articles) {
  chdir "$itemization_source/$article";
  my @items = `find text -name "ckb*.miz" | sed -e 's/\.miz//'`;
  chomp @items;

  foreach my $item (@items) {

    $item =~ /ckb([0-9]+)/;
    my $item_num = $1;

    my @needed = `$items_needed_script $item`;
    foreach my $needed (@needed) {
      my $aid = aid_attribute ($needed);
      my $aid_lc = lc $aid;
      if ($aid_lc =~ /ckb([0-9]+)/) {
	my $local_item_num = $1;
	print DEPGRAPH ("$article:$item_num $article:$local_item_num", "\n");
      } else {
	my $key;
	if ($needed =~ /Pattern/) {
	  my $pattern_nr = nr_attribute ($needed);
	  my $pattern_kind = kind_attribute ($needed);
	  my $pattern_kind_lc = lc $pattern_kind;
	  $key = "$aid_lc" . ":" . "$pattern_kind_lc" . "pattern" . ":" . $pattern_nr;
	} elsif ($needed =~ /Theorem/) {
	  my $kind = kind_attribute ($needed);
	  my $nr = nr_attribute ($needed);
	  if ($kind eq 'D') {
	    $key = "$aid_lc:deftheorem:$nr";
	  } else {
	    $key = "$aid_lc:theorem:$nr";
	  }
	} elsif ($needed =~ /Constructor/) {
	  my $nr = nr_attribute ($needed);
	  my $kind = kind_attribute ($needed);
	  my $kind_lc = lc $kind;
	  $key = "$aid_lc" . ":" . "$kind_lc" . "constructor" . ":" . $nr;
	} elsif ($needed =~ /Scheme/) {
	  my $nr = nr_attribute ($needed);
	  $key = "$aid_lc:scheme:$nr";
	} elsif ($needed =~ /Definiens/) {
	  my $nr = defnr_attribute ($needed);
	  my $constrkind = constrkind_attribute ($needed);
	  my $constrkind_lc = lc $constrkind;
	  $key = "$aid_lc" . ':' . "definiens" . ":" . "$nr";
	} elsif ($needed =~ /[RCF]Cluster/) {
	  my $nr = nr_attribute ($needed);
	  my $cluster_kind;
	  if ($needed =~ /RCluster/) {
	    $cluster_kind = 'r';
	  } elsif ($needed =~ /CCluster/) {
	    $cluster_kind = 'c';
	  } else {
	    $cluster_kind = 'f';
	  }
	  $key = "$aid_lc" . ":" . $cluster_kind . "cluster" . ":" . $nr;
	} elsif ($needed =~ /Identify/) {
	  my $nr = nr_attribute ($needed);
	  my $constrkind = constrkind_attribute ($needed);
	  my $constrkind_lc = lc $constrkind;
	  $key = "$aid_lc" . ':' . $constrkind_lc . "identification" . ":" . $nr;
	} else {
	  warn "weird: don't know how to handle items that look like '$needed'";
	}

	if (defined $key) {
	  my $maps_to = $mml_name_to_item_number{"$key"};
	  if (defined $maps_to) {
	    print DEPGRAPH ("$article:$item_num $maps_to", "\n");
	  } else {
	    warn "weird: there is no value for $key in the table (processing article $article item $item)";
	  }
	}
      }

    }
  }
}

######################################################################
### Hard-coded dependencies: HIDDEN and TARSKI
######################################################################

sub print_deps {
  my $item = shift;
  my @deps = @{shift ()};
  foreach my $dep (@deps) {
    print DEPGRAPH ("$item $dep", "\n");
  }
}

sub print_hidden_deps {
  my $item = shift;
  my @hidden_deps = @{shift ()};
  foreach my $hidden_dep (@hidden_deps) {
    my $fragment = $hidden_items{$hidden_dep};
    unless (defined $fragment) {
      die "There is no value for '$hidden_dep'!";
    }
    print DEPGRAPH ("$item $fragment", "\n");
  }
}

# hard-coded hidden dependencies

# loop!
# print_deps ('hidden:1', ['hidden:1']);
print_deps ('hidden:2', ['hidden:1']);
print_deps ('hidden:3', ['hidden:1']);
print_deps ('hidden:3', ['hidden:2']);
print_deps ('hidden:4', ['hidden:1']);

# hard-coded tarski dependencies

# 1.
#
# canceled

# depends on nothing

# 2.
#
# reserve x for set;
# reserve X for set;
# reserve Y for set;
#
# theorem
# (for x holds x in X iff x in Y) implies X = Y;

print_hidden_deps ('tarski:2', ['set:constructor',
				'set:pattern',
				'in:constructor',
				'in:pattern',
				'=:constructor',
				'=:pattern']);

# 3.
#
# reserve x for set;
# reserve y for set;
#
# definition
#   let y;
#   func
#     { y } -> set
#   means
#     x in it iff x = y;
#   correctness;
# end;

print_hidden_deps ('tarski:3', ['set:constructor',
				'set:pattern',
				'in:constructor',
				'in:pattern',
				'=:constructor',
				'=:pattern']);

# 4.
#
# reserve x for set;
# reserve y for set;
# reserve z for set;
#
# definition
#   let y, z;
#   func
#     { y, z } -> set
#   means
#     x in it iff x = y or x = z;
#   correctness;
#   commutativity;
# end;

print_hidden_deps ('tarski:4', ['set:constructor',
				'set:pattern',
				'in:constructor',
				'in:pattern',
				'=:constructor',
				'=:pattern']);

# 5.
#
# canceled

# depends on nothing

# 6.
#
# canceled

# depends on nothing

# 7.
#
# begin
#
# reserve x for set;
# reserve X for set;
# reserve Y for set;
#
# definition
#   let X,Y;
#   pred
#     X c= Y
#   means
#     x in X implies x in Y;
# end;

print_hidden_deps ('tarski:7', ['set:constructor',
				'set:pattern',
				'in:constructor',
				'in:pattern']);

# 8.
#
# reserve x for set;
# reserve X for set;
# reserve Y for set;
#
# definition
#   let X;
#   func
#     union X -> set
#   means
#     x in it iff ex Y st x in Y & Y in X;
#   correctness;
# end;

print_hidden_deps ('tarski:8', ['set:constructor',
				'set:pattern',
				'in:constructor',
				'in:pattern']);

# 9.
#
# canceled;

# depends on nothing

# 10.
#
# canceled;

# depends on nothing

# 11.
#
# reserve x for set;
# reserve X for set;
# reserve Y for set;
#
# theorem
# x in X implies ex Y st Y in X & not ex x st x in X & x in Y;

print_hidden_deps ('tarski:11', ['set:constructor',
				 'set:pattern',
				 'in:constructor',
				 'in:pattern']);

# 12.
#
# reserve x for set;
# reserve y for set;
# reserve z for set;
# reserve X for set;
#
# scheme Fraenkel { A()-> set, P[set, set] } :
#  ex X st for x holds x in X iff ex y st y in A() & P[y,x]
# provided
#  for x,y,z st P[x,y] & P[x,z] holds y = z
# proof
#   thus thesis;
# end;

print_hidden_deps ('tarski:12', ['set:constructor',
				 'set:pattern',
				 'in:constructor',
				 'in:pattern',
				 '=:constructor',
				 '=:pattern']);

# 13.
#
# reserve x for set;
# reserve y for set;
# 
# definition
#   let x,y;
#   func
#     [x,y]
#   means
#     it = { { x,y }, { x } };
#   correctness;
# end;

print_hidden_deps ('tarski:13', ['set:constructor',
				 'set:pattern',
				 '=:constructor',
				 '=:pattern']);

# this fragment depends on other items from TARSKI; it does not depend
# solely on HIDDEN: its environment is
#
# notations CKB3, CKB4;
# constructors CKB3, CKB4;

print_deps ('tarski:13', ['tarski:3', 'tarski:4']);

# 14.
#
# canceled;

# depends on nothing

# 15.
#
# reserve X for set;
# reserve Y for set;
# reserve Z for set;
# reserve x for set;
# reserve y for set;
# reserve z for set;
# reserve u for set;
#
# definition
#   let X,Y;
#   pred
#     X,Y are_equipotent
#   means
#     ex Z st
#       (for x st x in X ex y st y in Y & [x,y] in Z) &
#       (for y st y in Y ex x st x in X & [x,y] in Z) &
#       for x,y,z,u st [x,y] in Z & [z,u] in Z holds x = z iff y = u;
# end;

print_hidden_deps ('tarski:15', ['set:constructor',
				 'set:pattern',
				 'in:constructor',
				 'in:pattern',
				 '=:constructor',
				 '=:pattern']);

# this fragment does not depend solely on HIDDEN; its environment is:
#
# notations CKB13;
# constructors CKB13;

print_deps ('tarski:15', ['tarski:13']);

# 16.
#
# reserve M for set;
# reserve N for set;
# reserve X for set;
# reserve Y for set;
# reserve Z for set;
#
# theorem
#   ex M st N in M &
#      (for X,Y holds X in M & Y c= X implies Y in M) &
#      (for X st X in M ex Z st Z in M & for Y st Y c= X holds Y in Z) &
#      (for X holds X c= M implies X,M are_equipotent or X in M);

print_hidden_deps ('tarski:16', ['set:constructor',
				 'set:pattern',
				 'in:constructor',
				 'in:pattern']);

# also:
#
# notations CKB7, CKB15;
# constructors CKB7, CKB15;

print_deps ('tarski:16', ['tarski:7', 'tarski:15']);

######################################################################
### Done printing hard-coded HIDDEN and TARSKI dependencies
######################################################################

close DEPGRAPH
  or die "Couldn't close output filehandle at '$ckb_ckb_depgraph': $!";

# Now print the table that maps mizar items to our CKB's
open (ITEM_TO_CKB_TABLE, '>', $mizar_item_to_ckb)
  or die "Couldn't open an output filehandle at '$mizar_item_to_ckb': $!";
foreach my $key (keys %mml_name_to_item_number) {
  print ITEM_TO_CKB_TABLE ($key, ' ', $mml_name_to_item_number{$key}, "\n");
}
close ITEM_TO_CKB_TABLE
  or die "Couldn't close output filehandle at '$ckb_ckb_depgraph";

exit 0;
