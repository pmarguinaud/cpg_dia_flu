package FieldAPI;
#
use strict;
use FileHandle;
use Data::Dumper;
use Storable;
use Memoize;
use Fxtran;


sub getFieldAPIMember
{
  my $TI = shift;

  my $t = $TI;

  for my $x (@_)
    {
      if (exists ($t->{$x}))
        {
          my $ref = ref ($t->{$x});
          if ($ref && ($ref eq 'HASH'))
            {
              $t = $t->{$x};
            }
          elsif ($ref && ($ref eq 'ARRAY'))
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

sub pointers2FieldAPIPtr
{
  my $d = shift;

  my $TI = &Storable::retrieve ('types.dat');
  
  my @T = sort keys (%$TI);
  
  for my $T (@T)
    {
      my @F = &F ('.//T-decl-stmt[./_T-spec_/derived-T-spec[string(T-N)="?"]]//EN-decl/EN-N/N/n/text()', $T, $d, 1);
      for my $F (@F)
        {
          my @expr = &F ('.//named-E[string(N)="?"][R-LT]', $F, $d);
          for my $expr (@expr)
            {
              my @ct = &F ('./R-LT/component-R/ct', $expr);
              my $fd = &getFieldAPIMember ($TI, $T, map ({ $_->textContent } @ct));
              next unless ($fd);
  
              my ($f, $d) = @$fd;
  
              $ct[-1]->replaceNode (my $ct = &n ("<ct>$f</ct>"));
              my ($rlt) = &F ('./R-LT', $expr);
              $rlt->insertAfter (my $ptr = &n ("<component-R>%<ct>PTR</ct></component-R>"), $ct->parentNode);
  
              my ($ar) = &F ('following-sibling::ANY-R', $ptr);
  
              unless ($ar)
                {
                  # Add array reference
                  $ar = &n ('<array-R>(<section-subscript-LT><section-subscript>:</section-subscript></section-subscript-LT>)</array-R>');
                  $ptr->parentNode->insertAfter ($ar, $ptr);
                }
  
              # Add block number as last index
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


1;
