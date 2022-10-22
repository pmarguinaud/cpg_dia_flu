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
use Inline;
use Decl;
use Construct;
use Parallel;

my ($f) = @ARGV;

my $d = &Fxtran::fxtran (location => $f, fopts => [qw (-line-length 500)]);

&Decl::forceSingleDecl ($d);
#&Parallel::wrapArrays ($d);

&Parallel::makeParallel ($d);

print $d;

#'FileHandle'->new (">$f")->print ($d->textContent ());


