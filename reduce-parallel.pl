#!/usr/bin/perl -w

use strict;
use XML::LibXML;

# set up the ramdisk and itemization source
my $ramdisk = "/dev/shm";
my $harddisk = "/local/data/alama";

# sanity
unless (-e $ramdisk) {
  die "We can't carry out computations in the ramdisk at '$ramdisk' because there's no file or directory there!";
}
unless (-d $ramdisk) {
  die "We can't carry out computations in the ramdisk at '$ramdisk' because that's not actually a directory!";
}
unless (-r $ramdisk) {
  die "We can't carry out computations in the ramdisk at '$ramdisk' because the directory isn't readable!";
}
unless (-e $harddisk) {
  die "We can't grab articles from the itemization source at '$harddisk' because there is no file or directory there!";
}
unless (-d $harddisk) {
  die "We can't grab articles from the itemization source at '$harddisk' that is not actually a directory!";
}
unless (-r $harddisk) {
  die "We can't grab articles from the itemization source at '$harddisk' that directory isn't readable!";
}
unless (-w $harddisk) {
  die "We can't write the result of brutalizing articles under  '$harddisk' because that directory isn't writeable!";
}

sub article_on_harddisk {
  my $article = shift;
  return "$harddisk/$article";
}

sub article_in_ramdisk {
  my $article = shift;
  return "$ramdisk/$article";
}

my $reduce_item_script = '/home/alama/reduce-item.pl';
my $reduce_article_script = '/home/alama/mizar-items/reduce.pl';

# sanity
unless (-e $reduce_item_script) {
  die "Couldn't find the reduce-item script at '$reduce_item_script'!";
}
unless (-x $reduce_item_script) {
  die "The reduce-item script at '$reduce_item_script' is not executable!";
}
unless (-e $reduce_article_script) {
  die "Couldn't find the reduce-article script at '$reduce_article_script'!";
}
unless (-x $reduce_article_script) {
  die "The reduce-article script at '$reduce_article_script' is not executable!";
}

my @articles = ();

while (defined (my $article = <STDIN>)) {
  chomp $article;
  push (@articles, $article);
}

# sanity: all the articles exist
foreach my $article (@articles) {
  my $article_on_harddisk = article_on_harddisk ($article);
  unless (-e $article_on_harddisk) {
    die "The directory '$article_on_harddisk' for article '$article' doesn't exist!";
  }
  unless (-d $article_on_harddisk) {
    die "The directory '$article_on_harddisk' for article '$article' is not actually a directory!";
  }
  unless (-r $article_on_harddisk) {
    die "The directory '$article_on_harddisk' for article '$article' is not readable!";
  }
  my $article_text_dir = $article_on_harddisk . '/text';
  unless (-e $article_text_dir) {
    die "The text subdirectory '$article_text_dir' for article '$article' doesn't exist!";
  }
  unless (-d $article_text_dir) {
    die "The text subdirectory '$article_text_dir' for article '$article' is not actually a directory!";
  }
  unless (-r $article_text_dir) {
    die "The text subdirectory '$article_text_dir' for article '$article' is not readable!";
  }
  my @one_item = `find $article_text_dir -name 'ckb1.miz' | head -n 1`;
  if (@one_item == 0) {
    die "The text subdirectory '$article_text_dir' for article '$article' does not contain any items!";
  }
  # other sanity checks could be performed here, such as checking that all items are readable, but we'll skip that
}

# build the list of all items, which will eventually be passed to parallel
my @num_items = ();
foreach my $article (@articles) {
  my $article_on_harddisk = article_on_harddisk ($article);
  my $num_items_for_article = `find $article_on_harddisk/text -name "ckb*.miz" | wc --lines`;
  chomp $num_items_for_article;
  push (@num_items, $num_items_for_article);
}

# Make sure we don't max out the ramdisk
my @stuff_in_ramdisk = `find $ramdisk -mindepth 1 -maxdepth 1 -type d`;
if (@stuff_in_ramdisk > 500) {
  die "Failure: ramdisk is full; refusing to proceed";
}

# sanity check: the article isn't already in the ramdisk
foreach my $article (@articles) {
  my $article_in_ramdisk = article_in_ramdisk ($article);
  if (-e $article_in_ramdisk) {
    die "Failure: $article is already in the ramdisk -- why?";
  }
}

foreach my $article (@articles) {
  my $article_on_harddisk = article_on_harddisk ($article);
  my $article_in_ramdisk = article_in_ramdisk ($article);
  if (system ('cp', '-Rf', $article_on_harddisk, $ramdisk) != 0) {
    system ('rm', "-Rf", $article_in_ramdisk); # trash whatever is there
    die "Failure: error copying $article to the ramdisk: $!";
  }
}

# brutalize the original articles
# foreach my $article (@articles) {
#   warn "brutalizing article $article...";
#   my $brutalization_exit_code
#     = system ("$reduce_item_script $article");
#   $brutalization_exit_code = $brutalization_exit_code >> 8;
#   unless ($brutalization_exit_code == 0) {
#     die "We failed to brutalize article $article!";
#   }
# }

# now use the result of each of the article brutlizations as an
# initial approxiamation of the brutalization of each of the article's
# items
#
# we don't need to consider the eth (theorem) and esh (scheme) files
# -- the theorems and schemes environments for each item have already
# been suitably minimiized thanks to irrths.  But for the other kinds
# of imported items (definiens, notations, clusters, identifications),
# we will carry out the desired computation.
#
# How to proceed: for each extenion EXT, slurp the contents of
# ARTICLE-ENV-FILE having the extension EXT, if it exists.  For each
# item I, open the environment file ENV-FILE associated with I and
# EXT.  Select all those XML lines of ENV-FILE that mention a "ckb";
# these will be kept: obviously, the original article says nothing
# about the ckb's, so their presence is a strong indication that they
# are needed.
#
# If the file ARTICLE-ENV-FILE doesn't exist (curious case, but it
# might happen, I suppose), but, for an item I, the ENV-FILE for that
# item *does* exist, then surely this file contains nothing but items
# imported only from ckb's.  Dont' touch such ENV-FILEs.

sub parse_xml {
  my $xml_path = shift;
  my $xml_elem = shift;

  if (-e $xml_path) {

    {
      open(XML, '<', $xml_path)
	or die "Unable to open an input filehandle for $xml_path!";
      local $/; $_ = <XML>;
      close(XML);
    }

    my ($xmlbeg,$xmlnodes,$xmlend) 
      = $_ =~ m/(.*?)([<]$xml_elem\b.*[<]\/$xml_elem>)(.*)/s; # multiline match
    return ($xmlbeg,$xmlnodes,$xmlend);
  }
}

my %xml_pattern_for_extension =
  (
   'dfs' => ['Definientia/Definiens'],
   'eno' => ['Notations/Pattern'],
   'ecl' => ['Registrations/CCluster',
	     'Registrations/FCluster',
	     'Registrations/RCluster'],
   'eid' => ['IdentifyRegistrations/Identify']
  );

sub ckb_node {
  my $node = shift;
  if ($node->exists ('@aid')) {
    my $aid = $node->findvalue ('@aid');
    $aid =~ /CKB[0-9]+/ ? return 1 : return 0;
  } else {
    return 0;
  }
}

sub first_non_blank_child {
  my $node = shift;
  my @children = $node->nonBlankChildNodes ();
  return $children[0];
}

# my @extensions = keys %xml_pattern_for_extension;

# foreach my $i (1 .. scalar @articles) {
#   my $article = $articles[$i - 1];
#   foreach my $extension (@extensions) {
#     my @patterns_for_extension = @{$xml_pattern_for_extension{$extension}};
#     my $prototype = $patterns_for_extension[0];
#     my $env_file_for_article 
#       = article_in_ramdisk ($article) . '/' . $article . '.' . $extension;
#     if (-e $env_file_for_article) {
#       my $num_items_for_article = $num_items[$i - 1];
#       my $parser = XML::LibXML->new ();
#       my $dom = $parser->parse_file ($env_file_for_article);
#       my ($toplevel_item) = split ('/', $prototype);
#       my $xpath_query = join (' | ', @patterns_for_extension);
#       my @article_nodes = $dom->findnodes ($xpath_query);
#       my $article_root_node = $dom->documentElement ();
#       foreach my $j (1 .. $num_items_for_article) {
# 	my $item_name = "ckb$j";
# 	my $env_file_for_item
# 	  = article_in_ramdisk ($article) . '/text/' . $item_name . '.' . $extension;
# 	if (-e $env_file_for_item) {
# 	  my $item_parser = XML::LibXML->new ();
# 	  my $item_dom = $item_parser->parse_file ($env_file_for_item);
# 	  my $item_root = $item_dom->documentElement ();

# 	  # kludge
# 	  my $item_root_mizfiles = $item_root->findvalue ('@mizfiles');
# 	  $item_root_mizfiles =~ s/\//&#47;/g;
# 	  $item_root->setAttribute ('mizfiles', $item_root_mizfiles);

# 	  my @item_nodes = $item_dom->findnodes ($xpath_query);
	  
#  	  # check that the item environment isn't already smaller than
# 	  # the whole article's environment
# 	  if (scalar @item_nodes < scalar @article_nodes) {
# 	    warn "environment file ckb$j.$extension is already smaller than the original article's environment -- not doing any further work";
# 	  } else {
# 	    # do some work
# 	    $item_root->removeChildNodes ();
	    
# 	    my $blank_node = $item_dom->createTextNode ("\n");
	    
# 	    $item_root->appendChild ($blank_node);
# 	    foreach my $article_node (@article_nodes) {
# 	      $item_root->appendChild ($article_node);
# 	      $item_root->appendChild ($blank_node);
# 	    }
	    
# 	    foreach my $item_node (@item_nodes) {
# 	      if (ckb_node ($item_node)) {
# 		$item_root->appendChild ($blank_node);
# 		$item_root->appendChild ($item_node);
# 	      }
# 	    }
	    
# 	    open (ITEM_XML, '>', $env_file_for_item)
# 	      or die "Couldn't open an output filehandle for $env_file_for_item!";
# 	    print ITEM_XML ($item_dom->toString (0));
# 	    close ITEM_XML
# 	      or die "Couldn't close the output filehandle for $env_file_for_item!";
# 	  }
# 	} else {
# 	  warn "Weird: the original article has a .$extension file, but item $j does not";
# 	}
#       }
#     }
#   }
# }

my $parallel_items = '';
foreach my $i (1 .. scalar @articles) {
  my $article = $articles[$i - 1];
  my $num_items_for_article = $num_items[$i - 1];
  foreach my $j (1 .. $num_items_for_article) {
    $parallel_items .= "$article:$j ";
  }
}

my $parallel_exit_code 
  = system ("parallel --jobs +0 --eta --progress --halt-on-error=1 '$reduce_item_script {} > /dev/null 2>&1' ::: $parallel_items");
$parallel_exit_code = $parallel_exit_code >> 8;
unless ($parallel_exit_code == 0) {
  die "parallel did not exit cleanly; its exit code was $parallel_exit_code"
}

print "Success: brutalization of articles @articles succeeded", "\n";

foreach my $article (@articles) {
  my $article_on_harddisk = article_on_harddisk ($article);
  my $article_in_ramdisk = article_in_ramdisk ($article);
  system ('rm', '-Rf', $article_on_harddisk);
  system ('mv', $article_in_ramdisk, $harddisk) == 0
    or die "Failure: error moving $article out of the ramdisk!";
}

exit 0;
