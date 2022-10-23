#!/usr/bin/perl -w

use strict;

use FileHandle;
use Data::Dumper;
use File::Basename;
use FindBin qw ($Bin);
use lib $Bin;

use Common;

use Fxtran;
use Parallel;

my ($f) = @ARGV;

my $d = &Fxtran::fxtran (location => $f, fopts => [qw (-line-length 500)]);

&Parallel::makeParallel ($d);

print $d->textContent;

'FileHandle'->new (">$f.new")->print ($d->textContent ());
'FileHandle'->new (">$f.new.xml")->print ($d->toString ());


