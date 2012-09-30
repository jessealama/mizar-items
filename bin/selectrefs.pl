## return three hash pointers - first for schemes, then theorems, then definitions
sub ParseRef
{
    my ($filestem) = @_;
    open(REF,">$filestem.refx") or die "Unable to open an output filehandle for $filestem.refx in directory $gtopdir: $!";
    my @refs = ({},{},{});
    my $i = 0;
    while(<REF>)
    {
	if(/syThreeDots/) 
	{
	    $_ = <REF>; 
	    m(/.*x=\"(\d+)\".*/) or die "bad REFX file";
	    my $articlenr = $1;
	    $_ = <REF>; 
	    m(/.*x=\"(\d+)\".*/) or die "bad REFX file";
	    my $refnr = $1;
	    <REF>; <REF>;
	    $refs[$i]->{"$articlenr:$refnr"} = ();
	}
	if(/sySemiColon/)
	{
	    $i++;
	}
    }
    close(REF);
    die "Wrong number of ref kinds: $i" if($i != 3);
    return \@refs;
}


## only callable with .eth or .esh; $refs is an array pointer of three hashes returned by ParseRef
sub PruneRefXML
{
    my ($xml_elem,$file_ext,$filestem,$refs) = @_;

    my ($schs, $ths, $defs) = @$refs;

    my $res = 0;


    my $xitemfile = $filestem . $file_ext;
    if (-e $xitemfile) {
      {
	open(XML, $xitemfile) or die "Unable to open an output filehandle for $xitemfile in directory $gtopdir!";
	local $/; $_ = <XML>;
	close(XML);
      }
    } else {
      print "nothing to trim for $xitemfile";
      return;
    }

    my ($xmlbeg,$xmlnodes,$xmlend) = $_ =~ m/(.*?)([<]$xml_elem\b.*[<]\/$xml_elem>)(.*)/s;
    if (defined $xmlbeg) 
    {
      ## call Mizar parser to get the tp positions
      my @xmlelems = $xmlnodes =~ m/(<$xml_elem\b.*?<\/$xml_elem>)/sg; # this is a multiline match

      open(XML1,">$filestem$file_ext") or die "Unable to open an output filehandle for $filestem$file_ext in directory $gtopdir: $!";
      print XML1 $xmlbeg;

      if($file_ext eq '.eth')
      {
	  foreach my $elemnr (0 .. scalar(@xmlelems)-1)
	  {
	      my $first_line = (split /\n/, $xmlelems[$elemnr] )[0];
	      
	      $first_line =~ m/.*articlenr=\"(\d+)\".* nr=\"(\d+)\".* kind=\"([DT])\"/ or die "bad element $first_line";
	      
	      my ($ref, $kind) = ( "$1:$2", $3);
	      my $needed = ($kind eq 'T')? $ths : $defs;
	      
	      if( exists $needed->{$ref})
	      {
		  print XML1 $xmlelems[$elemnr];
		  $res++;
	      }
	  }
      }
      elsif($file_ext eq '.esh')
      {
	  foreach my $elemnr (0 .. scalar(@xmlelems)-1)
	  {
	      my $first_line = (split /\n/, $xmlelems[$elemnr] )[0];	      
	      $first_line =~ m/.*articlenr=\"(\d+)\".* nr=\"(\d+)\".*/ or die "bad element $first_line";
	      
	      if( exists $schs->{"$1:$2"})
	      {
		  print XML1 $xmlelems[$elemnr];
		  $res++;
	      }
	  }
      }
      else { die "bad extension $file_ext"; }

      print XML1 $xmlend;
      close(XML1);
      return $res;
    }
}
