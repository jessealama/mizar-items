#!/usr/bin/perl -w

=head1 NAME

miz_item_deps_bf.pl [Options] XMLElementRegexp ExtensionOfImportedFile ArticleName

(get precise implicit Mizar dependencies on imported constructs using
a brute-force approach)

=head1 SYNOPSIS

# get Definiens dependencies

miz_item_deps_bf.pl -q Definiens dfs ~/test/a

# get RCluster,CCluster,FCluster dependencies

miz_item_deps_bf.pl -q \[RCF\]Cluster ecl ~/test/a1

 Options:
   --mizfiles=<arg>,        -m<arg>
   --verifier=<arg>,        -v<arg>
   --makeenv=<arg>,         -n<arg>
   --quiet,                 -q
   --help,                  -h
   --man

=head1 OPTIONS

=over 8


=item B<<< --mizfiles=<arg>, -m<arg> >>>

Sets the $MIZFILES environmental variable for Mizar processing.
The default is its value in the current environment.


=item B<<< --verifier=<arg>, -v<arg> >>>

Sets the verifier for Mizar processing.
The default is $MIZFILES/bin/verifier, and if that does not
exist, then just "verifier" (relying on $PATH).

=item B<<< --makeenv=<arg>, -n<arg> >>>

Sets the accommodator for Mizar processing.
The default is $MIZFILES/bin/makeenv, and if that does not
exist, then just "makeenv" (relying on $PATH).

=item B<<< --quiet, -q >>>

Run verifier with the quite flag.

=item B<<< --help, -h >>>

Print a brief help message and exit.

=item B<<< --man >>>

Print the manual page and exit.

=back

=head1 DESCRIPTION

This program tests by brute force approach which imported implicit
items (registrations, definitional expansions) are needed for a given
Mizar article. The longer-term goal is to do this for small pieces of
Mizar code rather than whole articles. This can be now emulated by
manually putting preceding article items into another article, or just
commenting them as much as possible.

=head1 TODO

The brute-force should be smarter. It should probabilistically (using
the machine-learning methods and previous data) rank the elements by
their expected need. They should be removed (or rather added) in this
order.

=head1 CONTACT

Josef Urban firstname.lastname(at)gmail.com

=head1 LICENCE

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

=cut

use strict;
use Cwd;
use Pod::Usage;
use Getopt::Long;
use XML::LibXML;
use File::Spec;

my ($gparallelize, $gerrorsonly,    $ganalyze,
    $ghtmlize,     $gtptpize,       $gmizfiles,
    $gverifier,    $gmakeenv,       $gtmpdir,
    $gxsldir,      $gmizhtml,       $gppolicy,
    $glonglines);

my ($gquiet, $help, $man);


Getopt::Long::Configure ("bundling");

GetOptions('parallelize|j=i'    => \$gparallelize,
	   'ppolicy|P=i'    => \$gppolicy,
	   'errorsonly|e=i'    => \$gerrorsonly,
	   'analyze|a'    => \$ganalyze,
	   'htmlize|M=i'    => \$ghtmlize,
	   'tptpize|t=i'    => \$gtptpize,
	   'mizfiles|m=s'    => \$gmizfiles,
	   'mizhtml|H=s'    => \$gmizhtml,
	   'xsldir|x=s'    => \$gxsldir,
	   'verifier|v=s'    => \$gverifier,
	   'makeenv|n=s'    => \$gmakeenv,
	   'tmpdir|T=s'      => \$gtmpdir,
	   'quiet|q'          => \$gquiet,
	   'longlines|l'      => \$glonglines,
	   'help|h'          => \$help,
	   'man'             => \$man)
    or pod2usage(2);

pod2usage(1) if($help);
pod2usage(-exitstatus => 0, -verbose => 2) if($man);

pod2usage(2) if ($#ARGV != 2);

my $gelem = shift(@ARGV);
my $gext = '.' . shift(@ARGV);

my ($gvolume,$gdirectories,$gfilestem) = File::Spec->splitpath( shift(@ARGV) );
if($gfilestem =~ m/(.*)[.]miz$/) { $gfilestem = $1;}



#chdir($gvolume . $gdirectories) if(defined($gdirectories) and !($gdirectories eq ''));
my $gtopdir = getcwd();

$gparallelize = 1 unless(defined($gparallelize));
$gppolicy = 1 unless(defined($gppolicy));
$gerrorsonly = 0 unless(defined($gerrorsonly));
$ghtmlize = 0 unless(defined($ghtmlize));
$gtptpize = 0 unless(defined($gtptpize));
$gmizfiles = $ENV{"MIZFILES"} unless(defined($gmizfiles));



unless(defined($gverifier))
{
    $gverifier = (-e "$gmizfiles/bin/verifier") ? "$gmizfiles/bin/verifier" : "verifier";
}

unless(defined($gmakeenv))
{
    $gmakeenv = (-e "$gmizfiles/bin/makeenv") ? "$gmizfiles/bin/makeenv" : "makeenv";
}

my $gaddfmsg = (-e "$gmizfiles/bin/addfmsg") ? "$gmizfiles/bin/addfmsg" : "addfmsg";
my $gerrflag = (-e "$gmizfiles/bin/errflag") ? "$gmizfiles/bin/errflag" : "errflag";

$gtmpdir = "" unless(defined($gtmpdir));

my $gquietflag = $gquiet ? ' -q ' : '';
my $gaflag = $ganalyze ? ' -a ' : '';
my $glflag = $glonglines ? ' -l ' : '';

$ENV{"MIZFILES"}= $gmizfiles;

my $pxext = '.parx';

my $miz = $gfilestem . ".miz";
my $xml = $gfilestem . ".xml";

## return the number of printed
sub PrepareXml
{
    my ($filestem,$file_ext,$xmlelems,$removed,$xmlbeg,$xmlend) = @_;
    my $res = 0;
    open(XML1,">$filestem$file_ext");
    print XML1 $xmlbeg;
    foreach my $elemnr (0 .. scalar(@$xmlelems) -1)
    {
	if(! exists $removed->{$elemnr})
	{
	    print XML1 $xmlelems->[$elemnr];
	    $res++;
	}
    }
    print XML1 $xmlend;
    close(XML1);
    return $res;
}

sub TestXMLElems ($$$)
{
    my ($xml_elem,$file_ext,$filestem) = @_;
    # print $filestem, "\n";
    # print $file_ext, "\n";
    # print $makeenv, "\n";
    # print getcwd(), "\n";

    die "makeenv errors"
      unless system ("$gmakeenv -l $filestem > /dev/null 2> /dev/null") == 0;

    my $xml_contents;
    my $xitemfile = $filestem . $file_ext;
    if (-e $xitemfile) {
      {
	open(XML, $xitemfile);
	local $/; $xml_contents = <XML>;
	close(XML);
      }
    } else {
      print "nothing to trim", "\n";
      return;
    }

    my ($xmlbeg,$xmlnodes,$xmlend) = $xml_contents
      =~ m/(.*?)([<]$xml_elem\b.*[<]\/$xml_elem>)(.*)/s;

    if (!defined $xmlbeg) {
      return;
    }

    ## call Mizar parser to get the tp positions
    my @xmlelems = $xmlnodes
      =~ m/(<$xml_elem\b.*?<\/$xml_elem>)/sg; # this is a multiline match

    # sanity
    die "Verification errors"
      unless system ("$gverifier -s -l -q $filestem > /dev/null 2>/dev/null") == 0;

    my %removed = (); ## indices of removed elements

    ## remove consecutive chunks of sqrt size and to retract to
    ## one-by-one if the chunk fails (not sure why better than
    ## logarithmic approach - perhaps simpler to write)
    my $total = scalar(@xmlelems);
    my $chunksize = 1 + int(sqrt($total));
    my $chunks = int($total / $chunksize);
    foreach my $chunk (0 .. $chunks)
    {
	foreach my $elem (0 .. $chunksize -1)
	{
	    $removed{$chunk * $chunksize + $elem} = 1;
	}
	PrepareXml($filestem,$file_ext,\@xmlelems,\%removed,$xmlbeg,$xmlend);
	if (system ("$gverifier -s -l -q $filestem > /dev/null 2>/dev/null") != 0)
	{
	    foreach my $elem (0 .. $chunksize -1)
	    {
		delete $removed{$chunk * $chunksize + $elem};
	    }
	    my $found = 0; ## when 1, at least one was found necessary already from these
	    foreach my $elem (0 .. $chunksize -1)
	    {
		## if the first condition is unmet, we know the last
		## elem is culprit and don't have to test
		if(!(($elem == $chunksize -1) && ($found == 0)) && 
		   ($chunk * $chunksize + $elem <= $#xmlelems))
		{
		    $removed{$chunk * $chunksize + $elem} = 1;
		    PrepareXml($filestem,$file_ext,\@xmlelems,\%removed,$xmlbeg,$xmlend);
		    if (system ("$gverifier -s -l -q $filestem > /dev/null 2>/dev/null") != 0)
		    {
			delete $removed{$chunk * $chunksize + $elem};
			$found = 1;
		    }
		}
	      }
	}
    }

    if (system ("$gverifier -s -l -q $filestem > /dev/null 2>/dev/null") != 0) {
      print "heuristic failed", "\n";
      %removed = ();
      foreach my $chunk (0 .. $chunks) {
    	foreach my $elem (0 .. $chunksize -1)
    	{
    	    $removed{$chunk * $chunksize + $elem} = 1;
    	}
    	PrepareXml($filestem,$file_ext,\@xmlelems,\%removed,$xmlbeg,$xmlend);
    	if (system ("$gverifier -s -l -q $filestem > /dev/null 2>/dev/null") != 0) {
    	  foreach my $elem (0 .. $chunksize -1) {
    	    delete $removed{$chunk * $chunksize + $elem};
    	  }
    	  my $found = 0; ## when 1, at least one was found necessary already from these
    	  foreach my $elem (0 .. $chunksize -1) {
    	    ## if the first condition is unmet, we know the last
    	    ## elem is culprit and don't have to test
    	    if (!(($elem == $chunksize -1) && ($found == 0)) && 
    		($chunk * $chunksize + $elem <= $#xmlelems)) {
    	      $removed{$chunk * $chunksize + $elem} = 1;
    	      PrepareXml($filestem,$file_ext,\@xmlelems,\%removed,$xmlbeg,$xmlend);
    	      if (system ("$gverifier -s -l -q $filestem > /dev/null 2>/dev/null") != 0) {
    		delete $removed{$chunk * $chunksize + $elem};
    		$found = 1;
    	      }
    	    }
    	  }
    	}
      }
    } else {
      print "heuristic succeeded!", "\n";
    }

    # foreach my $chunk (0 .. $#xmlelems)
    # {
    # 	$removed{$chunk} = 1;
    # 	PrepareXml($filestem,$file_ext,$xmlelems,$removed,$xmlbeg,$xmlend);
    # 	delete $removed{$chunk} if(system("$gverifier -l -q $filestem") !=0);
    # }

    ## print the final form
    my $needed
      = PrepareXml($filestem,$file_ext,\@xmlelems,\%removed,$xmlbeg,$xmlend);
    ## print stats

    print 'total ', $xml_elem, ': ', $total, "\n";
    print 'removed: ', $total - $needed, "\n";
    print 'needed: ', $needed, "\n";

}


TestXMLElems ($gelem, $gext, $gdirectories . $gfilestem);
