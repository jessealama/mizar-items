#!/usr/bin/perl -w

use strict;

my $itemization_source = '/tmp';
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

my $num_articles_handled = 10;
my @articles = ('tarski');
my @first_hundred = `head -n $num_articles_handled $mml_lar`;
chomp @first_hundred;
push (@articles, @first_hundred);

my %mml_name_to_item_number = ();
# keys are strings such as "xboole_0:deftheorem:1", values are string
# like "xboole_0:1", which means: xboole_0 item 1 gives rise to
# xboole_0 deftheorem #1.

# hard-coded hidden mappings
#
# 'set'
$mml_name_to_item_number{'hidden:mpattern:1'} = 'hidden:1';
$mml_name_to_item_number{'hidden:mconstructor:1'} = 'hidden:1';

# =
$mml_name_to_item_number{'hidden:rconstructor:1'} = 'hidden:2';
$mml_name_to_item_number{'hidden:rpattern:1'} = 'hidden:2';

# <>
$mml_name_to_item_number{'hidden:rpattern:2'} = 'hidden:3'; 

# in
$mml_name_to_item_number{'hidden:rconstructor:2'} = 'hidden:4';
$mml_name_to_item_number{'hidden:rpattern:3'} = 'hidden:4'; 

# hard-coded tarski mappings
$mml_name_to_item_number{'tarski:theorem:1'} = 'tarski:1';

$mml_name_to_item_number{'tarski:kconstructor:1'} = 'tarski:2';
$mml_name_to_item_number{'tarski:kdefiniens:1'} = 'tarski:2';
$mml_name_to_item_number{'tarski:kpattern:1'} = 'tarski:2';
$mml_name_to_item_number{'tarski:deftheorem:1'} = 'tarski:2';

$mml_name_to_item_number{'tarski:kconstructor:2'} = 'tarski:3';
$mml_name_to_item_number{'tarski:kpattern:2'} = 'tarski:3';
$mml_name_to_item_number{'tarski:kdefiniens:2'} = 'tarski:3';
$mml_name_to_item_number{'tarski:deftheorem:2'} = 'tarski:3';

$mml_name_to_item_number{'tarski:rconstructor:1'} = 'tarski:4';
$mml_name_to_item_number{'tarski:rdefiniens:1'} = 'tarski:4';
$mml_name_to_item_number{'tarski:rpattern:1'} = 'tarski:4';
$mml_name_to_item_number{'tarski:deftheorem:3'} = 'tarski:4';

$mml_name_to_item_number{'tarski:kconstructor:3'} = 'tarski:5';
$mml_name_to_item_number{'tarski:kdefiniens:3'} = 'tarski:5';
$mml_name_to_item_number{'tarski:kpattern:3'} = 'tarski:5';
$mml_name_to_item_number{'tarski:deftheorem:4'} = 'tarski:5';

$mml_name_to_item_number{'tarski:theorem:2'} = 'tarski:6';

$mml_name_to_item_number{'tarski:scheme:1'} = 'tarski:7';

$mml_name_to_item_number{'tarski:kconstructor:4'} = 'tarski:8';
$mml_name_to_item_number{'tarski:kdefiniens:4'} = 'tarski:8';
$mml_name_to_item_number{'tarski:kpattern:4'} = 'tarski:8';
$mml_name_to_item_number{'tarski:deftheorem:5'} = 'tarski:8';

$mml_name_to_item_number{'tarski:rconstructor:2'} = 'tarski:9';
$mml_name_to_item_number{'tarski:rdefiniens:2'} = 'tarski:9';
$mml_name_to_item_number{'tarski:rpattern:2'} = 'tarski:9';
$mml_name_to_item_number{'tarski:deftheorem:6'} = 'tarski:9';

$mml_name_to_item_number{'tarski:theorem:3'} = 'tarski:10';

# hacks: not sure why...
$mml_name_to_item_number{'tarski:rdefiniens:3'} = 'tarski:4';
$mml_name_to_item_number{'tarski:rdefiniens:6'} = 'tarski:9';
$mml_name_to_item_number{'tarski:kdefiniens:5'} = 'tarski:8';
$mml_name_to_item_number{'tarski:theorem:9'} = 'tarski:10';
$mml_name_to_item_number{'tarski:theorem:7'} = 'tarski:6';

# print ('tarski:kconstructor:4 ==> tarski:kconstructor:2');
# print ('tarski:rdefiniens:2 ==> tarski:kconstructor:4');
# print ('tarski:deftheorem:6 ==> tarski:kconstructor:4');

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

      my $constructor_line = `grep --max-count=1 '<Constructor ' $item_miz`;
      chomp $constructor_line;
      unless ($constructor_line eq '') {
	my $constructor_nr = nr_attribute ($constructor_line);
	my $constructor_kind = kind_attribute ($constructor_line);
	my $constructor_kind_lc = lc $constructor_kind;
	my $key = "$article_name" . ":" . "$constructor_kind_lc" . "constructor" . ":" . $constructor_nr;
	$mml_name_to_item_number{$key} = "$article_name:$item_num";
      }

      my $pattern_line = `grep --max-count=1 '<Pattern ' $item_miz`;
      chomp $pattern_line;
      unless ($pattern_line eq '') {
	  my $pattern_nr = nr_attribute ($pattern_line);
	  my $pattern_kind = kind_attribute ($pattern_line);
	  my $pattern_kind_lc = lc $pattern_kind;
	  my $key = "$article_name" . ":" . "$pattern_kind_lc" . "pattern" . ":" . "$pattern_nr";
	  $mml_name_to_item_number{$key} = "$article_name:$item_num";
      }

      my $definiens_line = `grep --max-count=1 '<Definiens ' $item_miz`;
      chomp $definiens_line;
      unless ($definiens_line eq '') {
	  my $defnr = defnr_attribute ($definiens_line);
	  my $constrkind = constrkind_attribute ($definiens_line);
	  my $constrkind_lc = lc $constrkind;
	  my $key = "$article_name" . ":" . "$constrkind_lc" . "definiens" . ":" . "$defnr";
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
      warn "Don't know how to handle an item that looks like '$second_line'";
    }
  }
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

# now compute the dependencies
foreach my $article (@articles) {
  chdir "$itemization_source/$article";
  my @items;
  if ($article eq 'tarski') {
    @items = ('tarski1', 'tarski2', 'tarski3', 'tarski4', 'tarski5',
	      'tarski6', 'tarski7', 'tarski8', 'tarski9', 'tarski10');
  } else {
    @items = `find text -name "ckb*.miz" | sed -e 's/\.miz//'`;
    chomp @items;
  }
  foreach my $item (@items) {
    my $item_num;

    if ($article eq 'tarski') {
      $item =~ /tarski([0-9]+)/;
      $item_num = $1;
    } else {
      $item =~ /ckb([0-9]+)/;
      $item_num = $1;
    }

    my @needed = `$items_needed_script $item`;
    foreach my $needed (@needed) {
      my $aid = aid_attribute ($needed);
      my $aid_lc = lc $aid;
      if ($aid_lc =~ /ckb([0-9]+)/) {
	my $local_item_num = $1;
	print "$article:$item_num $article:$local_item_num", "\n";
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
	  my $defnr = defnr_attribute ($needed);
	  my $constrkind = constrkind_attribute ($needed);
	  my $constrkind_lc = lc $constrkind;
	  $key = "$aid_lc" . ':' . "$constrkind_lc" . "definiens" . ":" . "$defnr";
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
	    print "$article:$item_num $maps_to", "\n";
	  } else {
	    warn "weird: there is no value for $key in the table (processing article $article item $item)";
	  }
	}
      }

    }
  }
}

# hard-coded hidden dependencies
# loop!
# print 'hidden:1 hidden:1';
print 'hidden:2 hidden:1', "\n";
print 'hidden:3 hidden:1', "\n";
print 'hidden:3 hidden:2', "\n";
print 'hidden:4 hidden:1', "\n";

# # hard-coded tarski dependencies

print 'tarski:1', ' ', $mml_name_to_item_number{'hidden:mpattern:1'}, "\n";
print 'tarski:1', ' ', $mml_name_to_item_number{'hidden:rpattern:1'}, "\n";
print 'tarski:1', ' ', $mml_name_to_item_number{'hidden:rpattern:2'}, "\n";
print 'tarski:1', ' ', $mml_name_to_item_number{'hidden:mconstructor:1'}, "\n";
print 'tarski:1', ' ', $mml_name_to_item_number{'hidden:rconstructor:1'}, "\n";
print 'tarski:1', ' ', $mml_name_to_item_number{'hidden:rconstructor:2'}, "\n";

print 'tarski:2', ' ', $mml_name_to_item_number{'hidden:mpattern:1'}, "\n";
print 'tarski:2', ' ', $mml_name_to_item_number{'hidden:rpattern:1'}, "\n";
print 'tarski:2', ' ', $mml_name_to_item_number{'hidden:rpattern:3'}, "\n";
print 'tarski:2', ' ', $mml_name_to_item_number{'hidden:mconstructor:1'}, "\n";
print 'tarski:2', ' ', $mml_name_to_item_number{'hidden:rconstructor:1'}, "\n";
print 'tarski:2', ' ', $mml_name_to_item_number{'hidden:rconstructor:2'}, "\n";

print 'tarski:3', ' ', $mml_name_to_item_number{'hidden:mpattern:1'}, "\n";
print 'tarski:3', ' ', $mml_name_to_item_number{'hidden:rpattern:1'}, "\n";
print 'tarski:3', ' ', $mml_name_to_item_number{'hidden:rpattern:3'}, "\n";
print 'tarski:3', ' ', $mml_name_to_item_number{'hidden:mconstructor:1'}, "\n";
print 'tarski:3', ' ', $mml_name_to_item_number{'hidden:rconstructor:1'}, "\n";
print 'tarski:3', ' ', $mml_name_to_item_number{'hidden:rconstructor:2'}, "\n";

print 'tarski:4', ' ', $mml_name_to_item_number{'hidden:mpattern:1'}, "\n";
print 'tarski:4', ' ', $mml_name_to_item_number{'hidden:rpattern:3'}, "\n";
print 'tarski:4', ' ', $mml_name_to_item_number{'hidden:mconstructor:1'}, "\n";
print 'tarski:4', ' ', $mml_name_to_item_number{'hidden:rconstructor:1'}, "\n";
print 'tarski:4', ' ', $mml_name_to_item_number{'hidden:rconstructor:2'}, "\n";

print 'tarski:5', ' ', $mml_name_to_item_number{'hidden:mpattern:1'}, "\n";
print 'tarski:5', ' ', $mml_name_to_item_number{'hidden:rpattern:3'}, "\n";
print 'tarski:5', ' ', $mml_name_to_item_number{'hidden:mconstructor:1'}, "\n";
print 'tarski:5', ' ', $mml_name_to_item_number{'hidden:rconstructor:1'}, "\n";
print 'tarski:5', ' ', $mml_name_to_item_number{'hidden:rconstructor:2'}, "\n";

print 'tarski:6', ' ', $mml_name_to_item_number{'hidden:mpattern:1'}, "\n";
print 'tarski:6', ' ', $mml_name_to_item_number{'hidden:rpattern:3'}, "\n";
print 'tarski:6', ' ', $mml_name_to_item_number{'hidden:mconstructor:1'}, "\n";
print 'tarski:6', ' ', $mml_name_to_item_number{'hidden:rconstructor:1'}, "\n";
print 'tarski:6', ' ', $mml_name_to_item_number{'hidden:rconstructor:2'}, "\n";

print 'tarski:7', ' ', $mml_name_to_item_number{'hidden:mpattern:1'}, "\n";
print 'tarski:7', ' ', $mml_name_to_item_number{'hidden:rpattern:1'}, "\n";
print 'tarski:7', ' ', $mml_name_to_item_number{'hidden:rpattern:3'}, "\n";
print 'tarski:7', ' ', $mml_name_to_item_number{'hidden:mconstructor:1'}, "\n";
print 'tarski:7', ' ', $mml_name_to_item_number{'hidden:rconstructor:1'}, "\n";
print 'tarski:7', ' ', $mml_name_to_item_number{'hidden:rconstructor:2'}, "\n";

print 'tarski:8', ' ', $mml_name_to_item_number{'hidden:mpattern:1'}, "\n";
print 'tarski:8', ' ', $mml_name_to_item_number{'hidden:rpattern:1'}, "\n";
print 'tarski:8', ' ', 'tarski:2', "\n";
print 'tarski:8', ' ', 'tarski:3', "\n";
print 'tarski:8', ' ', $mml_name_to_item_number{'hidden:rconstructor:2'}, "\n";

print 'tarski:9', ' ', $mml_name_to_item_number{'hidden:mpattern:1'}, "\n";
print 'tarski:9', ' ', $mml_name_to_item_number{'hidden:rpattern:2'}, "\n";
print 'tarski:9', ' ', $mml_name_to_item_number{'hidden:rpattern:2'}, "\n";
print 'tarski:9', ' ', 'tarski:8', "\n";
print 'tarski:9', ' ', $mml_name_to_item_number{'hidden:mconstructor:1'}, "\n";
print 'tarski:9', ' ', $mml_name_to_item_number{'hidden:rconstructor:1'}, "\n";
print 'tarski:9', ' ', $mml_name_to_item_number{'hidden:rconstructor:2'}, "\n";

print 'tarski:10', ' ', $mml_name_to_item_number{'hidden:mpattern:1'}, "\n";
print 'tarski:10', ' ', $mml_name_to_item_number{'hidden:rpattern:2'}, "\n";
print 'tarski:10', ' ', 'tarski:4', "\n";
print 'tarski:10', ' ', 'tarski:9', "\n";
print 'tarski:10', ' ', $mml_name_to_item_number{'hidden:mconstructor:1'}, "\n";
print 'tarski:10', ' ', $mml_name_to_item_number{'hidden:rconstructor:1'}, "\n";
print 'tarski:10', ' ', $mml_name_to_item_number{'hidden:rconstructor:2'}, "\n";

exit 0;
