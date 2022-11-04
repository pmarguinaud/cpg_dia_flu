#!/usr/bin/perl -w

use strict;

use FileHandle;
use Data::Dumper;
use File::Basename;
use FindBin qw ($Bin);
use lib $Bin;

use Common;

use Fxtran;
use FieldAPI;
use Parallel;
use Stack;

for my $f (@ARGV)
  {
    my $d = &Fxtran::fxtran (location => $f, fopts => [qw (-line-length 500)]);

    &Parallel::makeParallelSingleColumnFieldAPI ($d, stack => 1);
    &Decl::changeIntent ($d, 'YDCPG_BNDS', 'INOUT');
    &Stack::addStack ($d);

    my ($name) = &F ('.//subroutine-N', $d, 1);
    'FileHandle'->new ('>' . lc ($name) . '.F90')->print ($d->textContent);
  }


