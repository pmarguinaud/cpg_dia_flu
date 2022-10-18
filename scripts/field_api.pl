#!/home/gmap/mrpm/marguina/install/perl-5.32.0/bin/perl -w
#
use strict;
use FileHandle;
use Data::Dumper;
use File::Basename;

use FindBin qw ($Bin);
use lib $Bin;
use Fxtran;
use FieldAPI;
use Inline;
use Decl;
use Construct;

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

my $d = &Fxtran::fxtran (location => $f, fopts => [qw (-line-length 500)]);

&Decl::forceSingleDecl ($d);
&Construct::changeIfStatementsInIfConstructs ($d);
&Inline::inlineContainedSubroutines ($d);
&FieldAPI::fieldify ($d);
&Construct::removeEmptyConstructs ($d);

($f = &basename ($f)) =~ s/\.F90$/_plan.F90/o;

'FileHandle'->new (">$f")->print ($d->textContent ());


