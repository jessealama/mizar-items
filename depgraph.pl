#!/usr/bin/perl -w

use strict;

sub nr_attribute {
  my $xml_line = shift;
  $xml_line =~ /nr=\"([0-9]+)\"/;
  return $1;
}

sub schemenr_attribute {
  my $xml_line = shift;
  $xml_line =~ /schemenr=\"([0-9]+)\"/;
  return $1;
}

sub kind_attribute {
  my $xml_line = shift;
  $xml_line =~ /kind=\"(\w)\"/;
  return $1;
}

my @articles = `head -n 10 /mnt/sdb3/alama/7.11.07_4.156.1112/mml.lar`;
chomp @articles;

my %mml_name_to_item_number = ();
# keys are strings such as "xboole_0:deftheorem:1", values are string
# like "xboole_0:1", which means: xboole_0 item 1 gives rise to
# xboole_0 deftheorem #1.

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

    chdir "/mnt/sdb3/alama/itemization/$article_name";
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
      my $third_line = `head -n 3 $item_miz | tail -n 1`;
      chomp $third_line;
      my $nr = nr_attribute ($third_line);
	$mml_name_to_item_number{"$article_name:theorem:$nr"}
	  = "$article_name:$item_num";
    } elsif ($second_line =~ /DefinitionBlock/) {

      # find any constructor, pattern, definiens and deftheorem

      my $constructor_line = `grep --max-count=1 '<Constructor ' $item_miz`;
      chomp $constructor_line;
      unless ($constructor_line eq '') {
	  my $constructor_nr = nr_attribute ($constructor_line);
	  $mml_name_to_item_number{"$article_name:constructor:$constructor_nr"}
	  = "$article_name:$item_num";
      }

      my $pattern_line = `grep --max-count=1 '<Pattern ' $item_miz`;
      chomp $pattern_line;
      unless ($pattern_line eq '') {
	  my $pattern_nr = nr_attribute ($pattern_line);
	  $mml_name_to_item_number{"$article_name:pattern:$pattern_nr"}
	  = "$article_name:$item_num";
      }

      my $definiens_line = `grep --max-count=1 '<Definiens ' $item_miz`;
      chomp $definiens_line;
      unless ($definiens_line eq '') {
	  my $definiens_nr = nr_attribute ($definiens_line);
	  $mml_name_to_item_number{"$article_name:definiens:$definiens_nr"}
	  = "$article_name:$item_num";
      }

      my $deftheorem_line = `grep --max-count=1 '<DefTheorem ' $item_miz`;
      chomp $deftheorem_line;
      unless ($deftheorem_line eq '') {
	  if (defined $deftheorem_line && ! $deftheorem_line eq '') {
	      my $deftheorem_nr = nr_attribute ($deftheorem_line);
	      $mml_name_to_item_number{"$article_name:deftheorem:$deftheorem_nr"}
	      = "$article_name:$item_num";
	  }
      }

    } elsif ($second_line =~ /SchemeBlock/) {
      my $schemenr = schemenr_attribute ($second_line);
      $mml_name_to_item_number{"$article_name:scheme:$schemenr"}
	= "$article_name:$item_num";
    } elsif ($second_line =~ /RegistrationBlock/) {
      my $cluster_line = `grep --max-count=1 '<[RCF]Cluster ' $item_miz`;
      chomp $cluster_line;
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
    } elsif ($second_line =~ /NotationBlock/) {
      # BAD: I don't currently split notation blocks, so grabbing only
      # the first pattern is, in general, not correct because some
      # patterns might be ignored
      my $pattern_line = `grep --max-count=1 '<Pattern ' $item_miz`;
      chomp $pattern_line;
      my $pattern_nr = nr_attribute ($pattern_line);
      my $pattern_kind = kind_attribute ($pattern_line);
      my $pattern_kind_lc = lc $pattern_kind;
      my $key = $article_name . ":" . $pattern_kind_lc . "pattern" . ":" . $pattern_nr;
      $mml_name_to_item_number{$key}
	= "$article_name:$item_num";
    } else {
      warn "Don't know how to handle an item that looks like '$second_line'";
    }
  }
}

foreach my $article (@articles) {
  chdir "/mnt/sdb3/alama/itemization/$article";
  my @items = `find text -name "ckb*.miz"`;
  chomp @items;
  foreach my $item_num (1 .. scalar @items) {
    my $item = $items[$item_num - 1];
    # warn "about to register item $item of article $article...";
    register_item_for_article ($article, $item_num);
  }
}

foreach my $key (keys %mml_name_to_item_number) {
  my $val = $mml_name_to_item_number{$key};
  print "$key ==> $val", "\n";
}

exit 0;
