package Outline;

use strict;
use Fxtran;
use Associate;
use Directive;
use Decl;

use Data::Dumper;

sub sortArgs
{
  my ($d, @args) = @_;

  my @dum = &F ('.//dummy-arg-LT/arg-N', $d, 1);

  my %dum = map { ($dum[$_], $_) } (0 .. $#dum);

  my @d = grep {   defined ($dum{$_}) } @args;
  my @l = grep { ! defined ($dum{$_}) } @args;

  @d = sort { $dum{$a} <=> $dum{$b} } @d;
  @l = sort { $a cmp $b } @l;

  return (@d, @l);
}


sub renameVariables
{
  my ($Nl, $local) = @_;

  my %N2M;
  
  my %S = qw (Z P LL LD I K J K N K L LD YL YD R P Y Y);
  my @S = qw (Z LL I J N L YL R Y);
  
  my %M;

  for my $N (@{ $Nl })
    {
      for my $v (values (%S))
        {
          if (index ($N, $v) == 0)
            {
              $N2M{$N} = $N;
              $M{$N} = 1;
              last;
            }
        }
    }
  
  for my $N (@{ $Nl })
    {
      next if ($N2M{$N});
      for my $k (@S)
        {
          if (index ($N, $k) == 0)
            {
              my $v = $S{$k};
              my $M = $N;
              $M =~ s/^$k/$v/ unless ($local->{$N});
              while ($M{$M})
                {
                  $M .= '_';
                }
              $N2M{$N} = $M;
  
              last;
            }
        }
    }
  
  return \%N2M;
} 


my @INTRINSIC = qw (SIGN MAX MIN MOD REAL EXP ASIN FOLH SQRT PRESENT ABS TINY SUM);
my %INTRINSIC = map { ($_, 1) } @INTRINSIC;

sub outlineSection
{
  my $d = shift;
  my %args = @_;

  my ($s) = @args{qw (section)};

  my $NAME = $args{name};

  ($NAME) = &F ('./@name', $s, 1) unless ($NAME);

  my $name = lc ($NAME);


  # Add include for outlined subroutine

  my ($include) = &F ('.//include[last()]', $d);
  if ($include)
    {
      $include->parentNode->insertAfter (&n ("<include>#include &quot;<filename>$name.intfb.h</filename>&quot;</include>"), $include);
      $include->parentNode->insertAfter (&t ("\n"), $include);
    }
  
  # Subroutine template

  my $o = &Fxtran::fxtran (string => << "EOF");
SUBROUTINE $NAME ()

USE PARKIND1, ONLY : JPIM, JPRB

!

IMPLICIT NONE

!

END SUBROUTINE
EOF
  
  &Associate::resolveAssociates ($s, outer => 1);
  
  my (%N, %do, %call); # Symbols, do variables, called subroutines

  my @expr = &F ('.//named-E', $s);
  for my $expr (@expr)
    {
      my ($n) = &F ('./N/n', $expr, 1);
      next if ($INTRINSIC{$n});
      if ($expr->parentNode->nodeName eq 'do-V')
        {
          $do{$n} = 1;
        }
      elsif ($expr->parentNode->nodeName eq 'procedure-designator')
        {
          $call{$n} = 1;
        }
      $N{$n} = 1;
    }
  
  my @N = &sortArgs ($d, grep { (! $do{$_}) && (! $call{$_}) } keys (%N));
  
  my %local; # Local to outlined (not used in original subroutine)

  for my $N (@N)
    {
      my @expr_d = &F ('.//named-E[string(N)="?"]', $N, $d); 
      my @expr_n = &F ('.//named-E[string(N)="?"]', $N, $s);
      $local{$N} = @expr_d > @expr_n ? 0 : 1;
      if (my ($decl) = &F ('.//T-decl-stmt[.//EN-decl[string(EN-N)="?"]]', $N, $d))
        {
           my ($intent) = &F ('./attribute[string(attribute-N)="INTENT"]', $decl);
           if ($intent)
             {
               $local{$N} = 0;
             }
        }
      else
        {
          die $N;
        }
    }
  
  my %rank = map { ($N[$_], $_) } (0 .. $#N);
  
  @N = sort { ($local{$a} <=> $local{$b}) or ($rank{$a} <=> $rank{$b}) } @N;
  
  my $N2M = &renameVariables (\@N, \%local);
  
  # Targets for use & declarations insertion

  my ($C1, $C2) = &F ('.//C', $o);
  
  # Insert section contents

  for my $node (&F ('./node()', $s))
    {
      $C2->parentNode->insertAfter ($node, $C2);
    }
  
  # Replace section by call statement in original subroutine

  my $call = &s ("CALL $NAME (" . join (', ', map { $_ } grep { ! $local{$_} } @N) . ')');
  $s->replaceNode ($call);

  my @decl;

  my ($dummy_arg_LT) = &F ('.//dummy-arg-LT', $o);
  for my $N (@N)
    {
      $N2M->{$N} or next;
  
      # Variable has to be passed by argument

      if (! $local{$N})
        {
          $dummy_arg_LT->appendChild (&n ("<arg-N><N><n>$N2M->{$N}</n></N></arg-N>"));
          $dummy_arg_LT->appendChild (&t (", ")) if ($N ne $N[-1]);
        }
  
      my @n = &F ('.//named-E[string(N)="?"]/N/n/text()', $N, $o);
      for my $n (@n)
        {
          $n->replaceNode (&t ($N2M->{$N}));
        }
  
      # Copy declaration from original subroutine
      if (my ($decl) = &F ('.//T-decl-stmt[.//EN-decl[string(EN-N)="?"]]', $N, $d))
        {
          $decl = $decl->cloneNode (1);
          my ($n) = &F ('.//EN-decl[string(EN-N)="?"]/EN-N/N/n/text()', $N, $decl);
          my @ts = &F ('.//derived-T-spec/T-N', $decl, 1);
  
          for my $ts (@ts)
            {
              my ($use) = &F ('.//use-stmt[.//use-N[string(.)="?"]]', $ts, $d);
              die $ts unless ($use);
              $use = $use->cloneNode (1);
              &Decl::use ($o, $use);
            }
  
          $n->replaceNode (&t ($N2M->{$N}));
          $C2->parentNode->insertBefore ($decl, $C2);
          $C2->parentNode->insertBefore (&t ("\n"), $C2);

        }
      else # Variable from module : guess its type from DOCTOR rules
        {
          if ($N =~ m/^[KIJMN]/o)
            {
              push @decl, "INTEGER (KIND=JPIM), INTENT (IN) :: $N2M->{$N}";
            }
          elsif ($N =~ m/^L/o)
            {
              push @decl, "LOGICAL, INTENT (IN) :: $N2M->{$N}";
            }
          elsif ($N =~ m/^R/o)
            {
              push @decl, "REAL (KIND=JPRB), INTENT (IN) :: $N2M->{$N}";
            }
          else
            {
              die $N;
            }
        }
  
    }

  &Decl::declare ($o, @decl);
  
  # Add required interfaces in new routine

  for my $call (sort keys (%call))
    {
      $C2->parentNode->insertBefore (&Fxtran::fxtran (statement => '#include "' . lc ($call) . '.intfb.h"'), $C2);
      $C2->parentNode->insertBefore (&t ("\n"), $C2);
    }
  
  $C2->parentNode->insertBefore (&t ("\n"), $C2);
  
  # Declare do variables

  for my $v (sort keys (%do))
    {
      $C2->parentNode->insertBefore (&Fxtran::fxtran (statement => "INTEGER (KIND=JPIM) :: $v"), $C2);
      $C2->parentNode->insertBefore (&t ("\n"), $C2);
    }
  
  # Remove targets
  $C1->unbindNode ();
  $C2->unbindNode ();
  
  # Remove unused variables in original routine
  
  for my $N (@N)
    {
      next unless ($local{$N});
      my ($en_decl) = &F ('.//EN-decl[string(EN-N)="?"]', $N, $d);
      die $N unless ($en_decl);
      my $stmt = &Fxtran::stmt ($en_decl);
      my ($intent) = &F ('./attribute[string(attribute-N)="INTENT"]', $stmt);
      unless ($intent)
        {
          $stmt->unbindNode ();
        }
    }
  
  return [$o, $call];
}


sub outline
{
  my $d = shift;
  
  my @s = &Directive::parseDirectives ($d);

  my @o;

  for my $s (@s)
    {
      push @o, &outlineSection ($d, section => $s);
    }

  return @o;
}

1;
