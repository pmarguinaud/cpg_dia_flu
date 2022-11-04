#!/usr/bin/perl -w

use strict;

use FileHandle;
use Data::Dumper;
use File::Basename;
use FindBin qw ($Bin);
use lib $Bin;

use Common;

use Fxtran;
use Outline;
use Decl;

my $f = shift;

my $d = &Fxtran::fxtran (location => $f, fopts => [qw (-line-length 500)]);

&Decl::forceSingleDecl ($d);
my @o = &Outline::outline ($d);

print &Dumper ([map { $_->textContent } ($d, @o)]);

