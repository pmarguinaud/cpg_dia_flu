#!/usr/bin/perl -w

use strict;

use FileHandle;
use File::Basename;
use FindBin qw ($Bin);

use lib $Bin;

use Common;
use Canonic;

use Fxtran;

for my $f (@ARGV)
  {
    my $d = &Fxtran::fxtran (location => $f, fopts => [qw (-line-length 500)]);
    'FileHandle'->new ('>' . &basename ($f) . '.xml')->print ($d->toString);
    &Canonic::makeCanonic ($d);
    'FileHandle'->new ('>canonic.' . &basename ($f) . '.xml')->print ($d->toString);
  }


