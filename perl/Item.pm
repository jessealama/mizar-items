use strict;
use Class::Std::Utils;
use base qw (Article);
{
  my %text;
  my %source_article;
  my %begin_line;
  my %begin_col;
  my %end_line;
  my %end_col;

  sub get_text {
    my ($self) = @_;
    defined $text{$self} ? return $text{$self}
                         : return extract_region ($begin_line{$self},
						  $begin_col{$self},
						  $end_line{$self},
						  $end_col{$self});
  }

  sub pretext {
    return '';
  }

  sub export {
    my ($self,$item_number) = @_;

    # copy the article environment
    my @vocabularies = $source_article{$self}->get_vocabularies ();
    my @notations = $source_article{$self}->get_notations ();
    my @constructors = $source_article{$self}->get_constructors ();
    my @registrations = $source_article{$self}->get_registrations ();
    my @requirements = $source_article{$self}->get_requirements ();
    my @definitions = $source_article{$self}->get_definitions ();
    my @theorems = $source_article{$self}->get_theorems ();
    my @schemes = $source_article{$self}->get_schemes ();

    # pad the copied environment with references to ALL earlier items
    # special case: vocabularies, requirements; don't pad these (we will
    # get errors from the mizar tools otherwise).
    my @earlier_items = map { "ITEM$_" } 1 .. $item_number - 1;
    push (@notations, @earlier_items);
    push (@constructors, @earlier_items);
    push (@registrations, @earlier_items);
    push (@definitions, @earlier_items);
    push (@theorems, @earlier_items);
    push (@schemes, @earlier_items);

    my $article = Article->new ();
    $article->set_name ("item$item_number");
    $article->set_vocabularies (\@vocabularies);
    $article->set_notations (\@notations);
    $article->set_constructors (\@constructors);
    $article->set_registrations (\@registrations);
    $article->set_requirements (\@requirements);
    $article->set_definitions (\@definitions);
    $article->set_theorems (\@theorems);
    $article->set_schemes (\@schemes);

    # now let's print this thing
    my $item_path = catfile ($article_text_dir, "item$number.miz");
    open my $item_miz, '>', $item_path
      or die "Unable to open an output filehandle at $item_path:\n\n  $!";
    
    print {$item_miz} 'environ', "\n";
    
    print {$item_miz}
      'vocabularies ', join (', ', @vocabularies), ';', "\n"
	unless (@vocabularies == 0);
    
    print {$item_miz}
      'notations ', join (', ', @notations), ';', "\n" 
	unless (@notations == 0);
    
    print {$item_miz}
      'constructors ', join (', ', @constructors), ';', "\n"
	unless (@constructors == 0);
    
    print {$item_miz}
      'registrations ', join (', ', @registrations), ';', "\n"
	unless (@registrations == 0);
    
    print {$item_miz}
      'requirements ', join (', ', @requirements), ';', "\n"
	unless (@requirements == 0);
    
    print {$item_miz}
      'definitions ', join (', ', @definitions), ';', "\n"
	unless (@definitions == 0);
    
    print {$item_miz}
      'theorems ', join (', ', @theorems), ';', "\n"
	unless (@theorems == 0);
    
    print {$item_miz}
      'schemes ', join (', ', @schemes), ';', "\n"
	unless (@schemes == 0);
    
    print {$item_miz} "\n";
    print {$item_miz} 'begin';
    print {$item_miz} "\n";

    # reservations
    my @reservations = @{reservations_before_line ($begin_line)};
    foreach my $reservation (@reservations) {
      print {$item_miz} 'reserve ', $reservation, "\n";
    }
    
    # the item proper
    print {$item_miz} $text;
    
    print {$item_miz} "\n"; # hygenic end-of-file newline
    
    close $item_miz
      or die "Unable to close the filehandle for the path $item_path";
    
   return; 
  }

}

