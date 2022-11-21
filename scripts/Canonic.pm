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
  if (my @drhook = &F ('./if-stmt[./action-stmt/call-stmt[string(procedure-designator)="DR_HOOK"]]', $pu))
    {
      ($ex1, $ex2) = ($drhook[0], $drhook[-1]);
    }
  else
    {
      die;
      my @node = &F ('./node()', $pu);
      for my $node (@node)
        {
          my $name = $node->nodeName;
          next unless ($name =~ m/-stmt$/o);
          print "$name\n";
        }
    }


  my $body = &n ('<body/>');
  $ex1->parentNode->insertBefore ($body, $ex1);

  for my $node ($ex1, &F ('following-sibling::node()', $ex1))
    {
      $body->appendChild ($node);
      last if ($ex2->unique_key eq $node->unique_key);
    }

  

}



1;
