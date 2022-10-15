#!/home/gmap/mrpm/marguina/install/perl-5.32.0/bin/perl -w
#
use strict;
use FileHandle;
use Data::Dumper;
use Storable;
use Memoize;

use FindBin qw ($Bin);
use lib $Bin;
use FieldAPI;
use Fxtran;

sub bt
{ 
  print @_; 

  print "\n";
  for (my $i = 0; ; $i++)
    {   
      last unless (my @call = caller ($i));
      print " $i ", $call[1], ':', $call[2], "\n";
    }   
  die "\n";
}

local $SIG{__WARN__} = \&bt;
local $SIG{__DIE__} = \&bt;


my ($f) = @ARGV;

my $d = &Fxtran::fxtran (location => $f);

&FieldAPI::pointers2FieldAPIPtr ($d);

print ($d->textContent ());
#'FileHandle'->new (">$f.new")->print ($d->textContent ());

