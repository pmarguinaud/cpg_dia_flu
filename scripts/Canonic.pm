package Canonic;

use strict;
use Fxtran;
use Data::Dumper;
use Intrinsic;
use Ref;

sub makeCanonic
{
  my $d = shift;

  for my $p (&F ('.//parens-R', $d))
    {
      my $expr = &Fxtran::expr ($p);

      if ($p->previousSibling)
        {
          &Ref::parensToArrayRef ($p);
          goto DONE;
        }

      my ($n) = &F ('./N', $expr, 1);
      if (&Intrinsic::isIntrinsic ($n))
        {
          $p->setNodeName ('function-R');
          goto DONE;
        }
      
      if (substr ($n, 0, 1) eq 'F')
        {
          $p->setNodeName ('function-R');
          goto DONE;
        }
 
      &Ref::parensToArrayRef ($p);

DONE:

    }


  my ($pu) = &F ('./object/file/program-unit', $d);

  my ($ex1, $ex2);
  if (my @drhook = &F ('./if-stmt/action-stmt/call-stmt[string(procedure-designator)="DR_HOOK"]', $pu))
    {
      ($ex1, $ex2) = ($drhook[0], $drhook[-1]);
    }
  else
    {
      my @node = &F ('./node()', $pu);
      for my $node (@node)
        {

        }
    }


}



1;
