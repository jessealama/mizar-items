package Article;
use strict;
use Class::Std::Utils;
use Mizar qw / fetch_directive /;
{

  my %name;
  my %path;
  my %text;

  # envionment
  my %vocabularies;
  my %notations;
  my %constructors;
  my %registrations;
  my %requirements;
  my %definitions;
  my %theorems;
  my %schemes;

  sub new {
    my ($class) = @_;
    my $new_object = bless \do{my $anon_scalar}, $class;
    return $new_object;
  }

  sub new_from_name_and_path {
    my ($class, $name, $path) = @_;

    my $new_object = bless \do{my $anon_scalar}, $class;

    $name{$new_object} = $name;

    my $filename = $path . $name . '.miz';

    # ensure that this exists
    unless (-e $filename) {
      die "There is no file called '$name.miz' under $path!";
    }

    # load the environment
    $vocabularies{$new_object} = fetch_directive ('Vocabularies');
    $notations{$new_object} = fetch_directive ('Notations');
    $constructors{$new_object} = fetch_directive ('Constructors');
    $registrations{$new_object} = fetch_directive ('Registrations');
    $requirements{$new_object} = fetch_directive ('Requirements');
    $definitions{$new_object} = fetch_directive ('Definitions');
    $theorems{$new_object} = fetch_directive ('Theorems');
    $schemes{$new_object} = fetch_directive ('Schemes');

    return $new_object;
  }

  sub get_vocabularies { my ($self) = @_; return $vocabularies{$self}; };
  sub set_vocabularies {
    my ($self,$vocabularies_ref) = @_;
    $vocabularies{$self} = $vocabularies_ref;
  }
  sub get_notations { my ($self) = @_; return $notations{$self}; }
  sub set_vocabularies {
    my ($self,$vocabularies_ref) = @_;
    $vocabularies{$self} = $vocabularies_ref;
  }

  sub get_constructors { my ($self) = @_; return $constructors{$self}; }
  sub set_vocabularies {
    my ($self,$constructors_ref) = @_;
    $constructors{$self} = $constructors_ref;
  }

  sub get_registrations { my ($self) = @_; return $registrations{$self}; }
  sub set_registrations {
    my ($self,$registrations_ref) = @_;
    $registrations{$self} = $registrations_ref;
  }

  sub get_requirements { my ($self) = @_; return $requirements{$self}; }
  sub set_requirements {
    my ($self,$requirements_ref) = @_;
    $requirements{$self} = $requirements_ref;
  }

  sub get_definitions { my ($self) = @_; return $definitions{$self}; }
  sub set_definitions {
    my ($self,$definitions_ref) = @_;
    $definitions{$self} = $definitions_ref;
  }

  sub get_theorems { my ($self) = @_; return $theorems{$self}; }
  sub set_theorems {
    my ($self,$theorems_ref) = @_;
    $theorems{$self} = $theorems_ref;
  }

  sub get_schemes { my ($self) = @_; return $schemes{$self}; }
  sub set_schemes {
    my ($self,$schemes_ref) = @_;
    $schemes{$self} = $schemes_ref;
  }

  sub get_name { my ($self) = @_; return $name{$self}; }
  sub set_name { 
    my ($self, $new_name) = @_;
    $name{$self} = $new_name;
    return $new_name;
  }

  sub get_text { my ($self) = @_; return $text{$self}; }
  sub set_text {
    my ($self, $new_text) = @_;
    $text{$self} = $new_text;
    return $new_text;
  }

  sub verify_in_directory {
    my ($self,$dir) = @_;
    $self->export ($dir);
    Mizar->set_workdir ($dir);
    Mizar->verify ($name{$self});
  }

  sub export {
    my ($self,$path) = @_;

    # sanity: don't overwrite pre-existing articles
    if (-e $path) {
      die "Refusing to overwrite article that already exists at $path";
    }

    my $article_name = $name{$self};

    # sanity: ensure that the article has a name
    unless (defined $article_name) {
      die "Cannot export an anonymous article!"
    }

    my $article_text = $text{$self};

    # build the environment
    my @vocabularies = get_vocabularies ();
    my @notations = get_notations ();
    my @constructors = get_constructors ();
    my @registrations = get_registrations ();
    my @requirements = get_requirements ();
    my @definitions = get_definitions ();
    my @theorems = get_theorems ();
    my @schemes = get_schemes ();

    # now let's print this thing
    my $item_path = catfile (Mizar->get_workdir (), "$article_name.miz");
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

    # the item proper
    print {$item_miz} $article_text;
    
    print {$item_miz} "\n"; # hygenic end-of-file newline
    
    close $item_miz
      or die "Unable to close the filehandle for the path $item_path";
    
   return; 
  }

}

1;
