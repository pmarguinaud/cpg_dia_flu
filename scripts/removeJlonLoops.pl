#!/home/gmap/mrpm/marguina/install/perl-5.32.0/bin/perl -w
#
use strict;
use FileHandle;
use Data::Dumper;

use FindBin qw ($Bin);
use lib $Bin;
use Fxtran;
use Loop;

my $f = shift;

my $d = &Fxtran::fxtran (location => $f, fopts => [qw (-line-length 300)]);

&Loop::removeJlonLoops ($d);

'FileHandle'->new (">$f.new")->print ($d->textContent);

