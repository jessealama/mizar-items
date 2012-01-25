use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME                => 'Mizar',
    AUTHOR              => 'Jesse Alama <jesse.alama@gmail.com>',
    VERSION_FROM        => 'lib/Mizar.pm',
    ABSTRACT_FROM       => 'lib/Mizar.pm',
    PL_FILES            => {},
    PREREQ_PM => {
        'Test::More' => 0,
        'version'    => 0,
    },
    dist                => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
    clean               => { FILES => 'Mizar-*' },
);
