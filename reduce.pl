#!/usr/bin/perl

use strict;

my %item_to_extension =
  (
   'Definiens' => 'dfs',
   'RCluster' => 'ecl',
   'CCluster' => 'ecl',
   'FCluster' => 'ecl',
   'Scheme' => 'esh',
   'Constructor' => 'atr',
   'Theorem' => 'eth',
   'Identify' => 'eid',
   'Pattern' => 'eno',
  );

my $article = $ARGV[0];

foreach my $item_kind (keys %item_to_extension) {
  my $extension = $item_to_extension{$item_kind};
  system ('/Users/alama/sources/mizar/mizar-items/miz_item_deps_bf.pl', $item_kind, $extension, $article) == 0
    or die "Whoops: something went wrong: $!";
  system ('cp', "$article.$extension", "$article-needed-$item_kind");
}

exit 0;
