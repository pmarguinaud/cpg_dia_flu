#!/home/gmap/mrpm/marguina/install/perl-5.32.0/bin/perl -w
#
use strict;
use FileHandle;
use Data::Dumper;
use Storable;
use Memoize;

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

&memoize ('getFieldAPIMember');


for my $T (@T)
  {
    my @F = &F ('.//T-decl-stmt[./_T-spec_/derived-T-spec[string(T-N)="?"]]//EN-decl/EN-N/N/n/text()', $T, $d, 1);
    for my $F (@F)
      {
        my @expr = &F ('.//named-E[string(N)="?"][R-LT]', $F, $d);
        for my $expr (@expr)
          {
            my @ct = &F ('./R-LT/component-R/ct', $expr);
            my $f = &getFieldAPIMember ($T, map ({ $_->textContent } @ct));
            next unless ($f);

            $ct[-1]->replaceNode (my $ct = &n ("<ct>$f</ct>"));
            my ($rlt) = &F ('./R-LT', $expr);
            $rlt->insertAfter (my $ptr = &n ("<component-R>%<ct>PTR</ct></component-R>"), $ct->parentNode);
            if (my ($ar) = &F ('following-sibling::ANY-R', $ptr))
              {
                if ($ar->nodeName eq 'parens-R')
                  {
                    my ($lt) = &F ('./element-LT', $ar);
                    $lt->appendChild (&t (','));
                    $lt->appendChild (&n ('<element><named-E><N><n>YDCPG_BNDS</n></N><R-LT>' 
                                        . '<component-R>%<ct>KBL</ct></component-R></R-LT></named-E>' 
                                        . '</element>'))
                  }
                elsif ($ar->nodeName eq 'array-R')
                  {
                    my ($lt) = &F ('./section-subscript-LT', $ar);
                    $lt->appendChild (&t (','));
                    $lt->appendChild (&n ('<section-subscript><lower-bound><named-E><N><n>YDCPG_BNDS</n></N><R-LT>' 
                                        . '<component-R>%<ct>KBL</ct></component-R></R-LT></named-E></lower-bound>' 
                                        . '</section-subscript>'))
                  }
                else
                  {
                    die $expr->textContent;
                  }
              }
            
          }
      }
  }



print ($d->textContent ());
#'FileHandle'->new (">$f.new")->print ($d->textContent ());

