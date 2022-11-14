package Directive;

use strict;
use Fxtran;

sub parseDirectives
{
# Add tags for each section

  my $d = shift;

  my @e;

  my @C = &F ('//C[starts-with(string (.),"!=")]', $d);
  
  while (my $C  = shift (@C))
    {
      (my $bdir = $C->textContent) =~ s/^!=\s*//o;

      ($bdir, my $opts) = ($bdir =~ m/^(\S+)(?:\s+(\S.*\S)\s*)?$/goms);

      my $noend = $bdir =~ s/=$//o;

      my %opts = $opts ? split (m/\s*[=,]\s*/o, $opts) : ();

      $bdir = lc ($bdir);
      my ($tag) = ($bdir =~ m/^(\w+)/o);

      my $Tag = $tag; 
      $Tag .= '-section' unless ($noend);

      my $e = &n ("<$Tag " . join (' ', map { sprintf ('%s="%s"', lc ($_), $opts{$_}) } keys (%opts))  . "/>");

      if (! $noend)
        {
          my @node;
          for (my $node = $C->nextSibling; ; $node = $node->nextSibling)
            {
              $node or die $C->textContent;
              if (($node->nodeName eq 'C') && (index ($node->textContent, '!=') == 0))
                {
                  my $C = shift (@C);
                  (my $edir = $C->textContent) =~ s/^!=\s*//o;
                  $edir = lc ($edir);

                  die unless ($edir =~ s/^end\s+//o);
                  die unless ($edir eq $tag);

                  $C->unbindNode ();
                  
                  last;
                }
              push @node, $node;
            }

          for my $node (@node)
            {
              $e->appendChild ($node);
            }
        }

      $C->replaceNode ($e);
      push @e, $e;

    }

  return @e;
}


1;
