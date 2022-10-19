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

my ($f) = @ARGV;

my $d = &Fxtran::fxtran (location => $f, fopts => [qw (-line-length 500)]);

&Decl::forceSingleDecl ($d);
&Construct::changeIfStatementsInIfConstructs ($d);
&Inline::inlineContainedSubroutines ($d);
&FieldAPI::fieldify ($d);
&Construct::removeEmptyConstructs ($d);

($f = &basename ($f)) =~ s/\.F90$/_plan.F90/o;

'FileHandle'->new (">$f")->print ($d->textContent ());


