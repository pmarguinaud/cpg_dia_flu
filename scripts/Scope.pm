package Scope;

use strict;
use Fxtran;

sub getNoExec
{
  my $d = shift;
 
  my ($exec) = grep { &Fxtran::stmt_is_executable ($_) } &F ('.//ANY-stmt', $d);
  my @prev = &F ('preceding::*', $exec);

  my $prev;
  for my $p (reverse (@prev))
    {
      next if ($p->nodeName eq '#text');
      next if ($p->nodeName eq 'C');
      $prev = $p;
      last;
    }

  $prev or die $d->textContent;

  my @anc = &F ('ancestor::*', $prev);

  for my $anc (reverse (@anc))
    {
      if (($anc->nodeName =~ m/-(?:construct|stmt)$/o) || ($anc->nodeName eq 'include'))
        {
          $prev = $anc;
        }
    }

  return $prev;
}


1;
