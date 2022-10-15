#!/home/gmap/mrpm/marguina/install/perl-5.32.0/bin/perl -w
#
use strict;
use FileHandle;
use Data::Dumper;
use Storable;

use FindBin qw ($Bin);
use lib $Bin;
use Fxtran;
use Inline;

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


my $T = &Storable::retrieve ('types.dat');

my ($f) = @ARGV;

my $d = &Fxtran::fxtran (location => $f);

my @T = qw (TCFU TXFU MF_PHYS_OUT_TYPE CPG_MISC_TYPE CPG_DYN_TYPE MF_PHYS_SURF_TYPE FIELD_VARIABLES);

sub getFieldAPIMember
{
  my $t = $T;

  for my $x (@_)
    {
      if (exists ($t->{$x}))
        {
          my $ref = ref ($t->{$x});
          if ($ref && ($ref eq 'HASH'))
            {
              $t = $t->{$x};
            }
          elsif (! $ref)
           {
             $t = $t->{$x};
           }
        }
      else
        {
          goto NOTFOUND;
        }
    }

  return $t;
NOTFOUND:
  return;
}


for my $T (@T)
  {
    my @F = &F ('.//T-decl-stmt[./_T-spec_/derived-T-spec[string(T-N)="?"]]//EN-decl/EN-N/N/n/text()', $T, $d, 1);
    for my $F (@F)
      {
        my @expr = &F ('.//named-E[string(N)="?"][R-LT]', $F, $d);
        for my $expr (@expr)
          {
            my @ct = &F ('./R-LT/component-R/ct', $expr, 1);
            my $f = &getFieldAPIMember ($T, @ct);
            print &Dumper ([$expr->textContent, $f]);
          }
      }
  }


__END__



print ($d->textContent ());
#'FileHandle'->new (">$f.new")->print ($d->textContent ());

