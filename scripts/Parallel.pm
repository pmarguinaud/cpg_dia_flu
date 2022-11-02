package Parallel;
#
use strict;
use FileHandle;
use Data::Dumper;
use Storable;
use List::MoreUtils qw (uniq);

use Scope;
use Fxtran;
use Ref;
use Decl;
use Subroutine;
use FieldAPI;
use OpenMP;


sub parseDirectives
{
# Add tags for each section

  my $d = shift;

  return unless (my @C = &F ('//C[starts-with(string (.),"!=")]', $d));
  
  while (my $C  = shift (@C))
    {
      (my $bdir = $C->textContent) =~ s/^!=\s*//o;

      ($bdir, my $opts) = ($bdir =~ m/^(\w+)\s*(?:\(\s*(\S.*\S)\s*\)\s*)?$/goms);
      my %opts = $opts ? split (m/\s*[=,]\s*/o, $opts) : ();

      $bdir = lc ($bdir);
      my ($tag) = ($bdir =~ m/^(\w+)/o);
  
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

      my $e = &n ("<$tag-section " . join (' ', map { sprintf ('%s="%s"', lc ($_), $opts{$_}) } keys (%opts))  . "/>");
 
      for my $node (@node)
        {
          $e->appendChild ($node);
        }

      $C->replaceNode ($e);

    }

  return 1;
}

sub tsToArray
{
  my ($ts, $nd) = @_;

  die unless ($ts->nodeName eq 'intrinsic-T-spec');
  my ($type) = &F ('./T-N', $ts, 1);
  my ($kind) = &F ('./K-selector/K-spec/named-E', $ts, 1);
  die unless ($type);

  if ($type eq 'LOGICAL')
    {
      die if ($kind);
      return "ARRAY_LOG${nd}D";
    }
  elsif ($type eq 'INTEGER')
    {
      die unless ($kind eq 'JPIM');
      return "ARRAY_INT${nd}D";
    }
  elsif ($type eq 'REAL')
    {
      die unless ($kind eq 'JPRB');
      return "ARRAY_${nd}D";
    }
  die;
}

sub wrapArrays
{
  my ($d, $suffix) = @_;

  my %args = map { ($_->textContent, $_) } &F ('.//subroutine-stmt/dummy-arg-LT/arg-N/N/n/text()', $d);

  my $noexec = &Scope::getNoExec ($d);

  # Find NPROMA arrays, record dimensions & change declaration statements

  my @KLON = qw (KLON YDCPG_OPTS%KLON);
  my @en_decl = &F ('.//T-decl-stmt//EN-decl[./array-spec/shape-spec-LT[' 
                  . join (' or ', map ({ "string(shape-spec)=\"$_\"" } @KLON))  . ']]', $d);


  my (@N, %Y, %L, %U);

  for my $en_decl (@en_decl)
    {
      my ($N) = &F ('./EN-N', $en_decl, 1);
   
      my $decl = &Fxtran::stmt ($en_decl);
      my @ss = &F ('./array-spec/shape-spec-LT/shape-spec', $en_decl);
      my $nd = scalar (@ss);

      my (@lb, @ub);
      for my $ss (@ss)
        {
          my ($lb) = &F ('./lower-bound/*', $ss); $lb ||= &e ('1'); push @lb, $lb;
          my ($ub) = &F ('./upper-bound/*', $ss);                   push @ub, $ub;
        }


      my ($ts) = &F ('./_T-spec_/*', $decl);

      my $class = &tsToArray ($ts, $nd + 1);

      my ($isArg) = &F ('.//attribute[string(attribute-N)="INTENT"]', $decl);

      my $Y = $isArg ? "YD_$N" : "YL_$N";
      $Y{$N} = $Y;
      $decl->replaceNode (&s ("TYPE ($class) :: $Y"));
  
      push @N, $N;
      $L{$Y} = [map { $_->textContent } @lb];
      $U{$Y} = [map { $_->textContent } @ub];

      # Change argument name

      if ($args{$N})
        {
          $args{$N}->setData ($Y);
        }
    }


  my ($lastst) = &F ('./object/file/program-unit/contains|./object/file/program-unit/end-subroutine-stmt', $d);

  # Initialize array wrappers

  $noexec->parentNode->insertAfter (&t ("\n"), $noexec);
  $lastst->parentNode->insertBefore (&t ("\n"), $lastst);

  for my $N (reverse (@N))
    {
      next if ($args{$N});
      my $Y = $Y{$N};
      my @lb = (@{ $L{$Y} }, 1); 
      my @ub = (@{ $U{$Y} }, 'YDCPG_OPTS%KGPBLKS');
      my $lbounds = (grep { $_ ne '1' } @lb) ? 'LBOUNDS=[' . join (',', @lb) . '], ' : '';
      my $ubounds =                            'UBOUNDS=[' . join (',', @ub) . ']';

      $noexec->parentNode->insertAfter (&s ("CALL $Y%INIT ($lbounds$ubounds, PERSISTENT=.TRUE.)"), $noexec);
      $noexec->parentNode->insertAfter (&t ("\n"), $noexec);
 
      $lastst->parentNode->insertBefore (&s ("CALL $Y%FINAL"), $lastst);
      $lastst->parentNode->insertBefore (&t ("\n"), $lastst);
 
    }

  $lastst->parentNode->insertBefore (&t ("\n"), $lastst);
  $noexec->parentNode->insertAfter (&t ("\n"), $noexec);

  # Use array wrappers in expression

  my %proc;

  my @expr = &F ('.//named-E[' . join (' or ', map { "string(N)=\"$_\"" } @N) . ']', $d);
  for my $expr (@expr)
    {
      my $stmt = &Fxtran::stmt ($expr);
      my ($para) = &F ('ancestor::parallel-section', $stmt);
      my $call = ($stmt->nodeName eq 'call-stmt') && $stmt;
      my @pu = &F ('ancestor::program-unit', $stmt); # Contained subroutines

      my ($N) = &F ('./N', $expr, 1); my $Y = $Y{$N};
      my $nd = scalar (@{ $L{$Y} });
      my ($n) = &F ('./N/n/text()', $expr); $n->setData ($Y);

      if ($para || scalar (@pu) > 1)
        {
          my $rlt = &Ref::getRLT ($expr);
          $rlt->insertBefore (&n ('<component-R>%<ct>P</ct></component-R>'), $rlt->firstChild);
        }

      if ($call && (! $para))
        {
          my ($proc) = &F ('./procedure-designator/named-E/N/n/text()', $stmt);
          $proc{$proc->unique_key} = $proc;
        }
    }

  for my $proc (values (%proc))
    {
      my $name = $proc->data;
      $proc->setData ($name . $suffix);

      next unless (my ($include) = &F ('.//include[string(filename)="?"]', lc ($name) . '.intfb.h', $d));

      if (&F ('.//call-stmt[string(procedure-designator)="?"]', $name, $d))
        {
          my $include_parallel = $include->cloneNode (1);
          my ($t) = &F ('./filename/text()', $include_parallel); 
          $t->setData (lc ($name) . lc ($suffix) . '.intfb.h');
          $include->parentNode->insertBefore ($include_parallel, $include);
          $include->parentNode->insertBefore (&t ("\n"), $include);
        }
      else
        {
          my ($t) = &F ('./filename/text()', $include); 
          $t->setData (lc ($name) . lc ($suffix) . '.intfb.h');
        }
    }
   
}

sub makeParallelView
{
  my $d = shift;

  return unless (&parseDirectives ($d));

  # Resolving ASSOCIATEs in parallel sections is mandatory
  &Associate::resolveAssociates ($d);

  &Decl::forceSingleDecl ($d);

  # Inlining contained subroutines called from parallel sections is mandatory
  # when they use some variables from the outer scope and these variables
  # are made FIRSTPRIVATE

  &Construct::changeIfStatementsInIfConstructs ($d);
  &Inline::inlineContainedSubroutines ($d);

  my $suffix = '_PARALLEL_VIEW';

  &wrapArrays ($d, $suffix);

  &Decl::declare ($d,  
                  'INTEGER (KIND=JPIM) :: IBL');
  &Decl::use ($d,
              'USE ARRAY_MOD');

  my (%T, %U);
  for my $en_decl (&F ('.//EN-decl', $d))
    {
      my ($stmt) = &Fxtran::stmt ($en_decl);
      next unless (my ($ts) = &F ('./_T-spec_/derived-T-spec', $stmt));
      my ($N) = &F ('./EN-N', $en_decl, 1);
      my ($T) = &F ('./T-N', $ts, 1);
      $T{$N} = $T;
      $U{$N} = &FieldAPI::isUpdatable ($T);
    }

  $U{YDCPG_BNDS} = 1;

  for my $U (sort keys (%U))
    {
      unless ($U{$U})
        {
          delete $U{$U};
          next;
        }
      next unless (my ($decl) = &F ('.//T-decl-stmt[.//EN-N[string(.)="?"]', $U, $d));
      next unless (my ($intent) = &F ('./attribute/intent-spec/text()', $decl));
      $intent->setData ('INOUT');
    }

  my @para = &F ('.//parallel-section', $d);

  for my $para (@para)
    {
      my @N = &uniq (grep { $U{$_} } &F ('.//named-E/N/n/text()',  $para, 1));

      my ($stmt) = &F ('.//ANY-stmt', $para);
      my $indent = ' ' x &Fxtran::getIndent ($stmt);

      # Insert loop nest

      my $loop = "DO IBL = 1, YDCPG_OPTS%KGPBLKS\n";
      for my $N (@N)
        {
          $loop .= "${indent}  CALL $N%UPDATE_VIEW (BLOCK_INDEX=IBL)\n";
        }
      $loop .= "${indent}ENDDO\n";

      my ($loop) = &Fxtran::fxtran (fragment => $loop);

      my ($enddo) = &F ('.//end-do-stmt', $loop);
      my $p = $enddo->parentNode;
     
      for my $node ($para->childNodes ()) 
        {   
          $p->insertBefore (&t (' ' x (2)), $enddo);
          &Fxtran::reIndent ($node, 2); 
          $p->insertBefore ($node, $enddo);
        }   
      $p->insertBefore (&t ($indent), $enddo);

      $para->appendChild ($loop);
      $para->insertBefore (&t ($indent), $loop);

      $para->parentNode->insertBefore (&t ("$indent"), $para);

      # Insert OpenMP directive

      my @priv = grep { ! $U{$_} } &F ('.//a-stmt/E-1/named-E/N|.//do-V/named-E/N', $para, 1);
      
      $para->insertBefore (&t ("\n"), $loop);
      &OpenMP::parallelDo ($loop, PRIVATE => \@priv, FIRSTPRIVATE => [sort keys (%U)]);
    }

  &Subroutine::addSuffix ($d, $suffix);
  
  return 1;
}

sub makeParallelFieldAPI
{
  use FieldAPI;

  my $d = shift;

  return unless (&parseDirectives ($d));

  # Resolving ASSOCIATEs in parallel sections is mandatory
  &Associate::resolveAssociates ($d);

  &Decl::forceSingleDecl ($d);

  # Inlining contained subroutines called from parallel sections is mandatory
  # when they use some variables from the outer scope and these variables
  # are made FIRSTPRIVATE

  &Construct::changeIfStatementsInIfConstructs ($d);
  &Inline::inlineContainedSubroutines ($d);

  my $suffix = '_PARALLEL_FIELD_API';

  &wrapArrays ($d, $suffix);

  &Decl::declare ($d,  
                  'INTEGER (KIND=JPIM) :: IBL');
  &Decl::use ($d,
              'USE ARRAY_MOD');

  &FieldAPI::pointers2FieldAPIPtr ($d);


  my @para = &F ('.//parallel-section', $d);

  for my $para (@para)
    {
      my ($stmt) = &F ('.//ANY-stmt', $para);
      my $indent = ' ' x &Fxtran::getIndent ($stmt);

      # Insert loop nest

      my $loop = "DO IBL = 1, YDCPG_OPTS%KGPBLKS\n";
      $loop .= "${indent}  CALL YDCPG_BNDS%UPDATE_VIEW (BLOCK_INDEX=IBL)\n";
      $loop .= "${indent}ENDDO\n";

      my ($loop) = &Fxtran::fxtran (fragment => $loop);

      my ($enddo) = &F ('.//end-do-stmt', $loop);
      my $p = $enddo->parentNode;
     
      for my $node ($para->childNodes ()) 
        {   
          $p->insertBefore (&t (' ' x (2)), $enddo);
          &Fxtran::reIndent ($node, 2); 
          $p->insertBefore ($node, $enddo);
        }   
      $p->insertBefore (&t ($indent), $enddo);

      $para->appendChild (&t ("\n"));
      $para->appendChild ($loop);
      $para->insertBefore (&t ($indent), $loop);

      &Call::addSuffix ($para, suffix => '_FIELD_API');

      # Insert OpenMP directive

      my @priv = &F ('.//a-stmt/E-1/named-E[not(.//component-R[string(ct)="PTR"])]/N|.//do-V/named-E/N', $para, 1);
      
      &OpenMP::parallelDo ($loop, PRIVATE => \@priv, FIRSTPRIVATE => ['YDCPG_BNDS']);

    }

  

  &Subroutine::addSuffix ($d, $suffix);

  return 1;
}

sub makeParallelSingleColumnFieldAPI
{
  use FieldAPI;
  use Loop;

  my $d = shift;
  my %opts = @_;

  return unless (&parseDirectives ($d));

  # Resolving ASSOCIATEs in parallel sections is mandatory
  &Associate::resolveAssociates ($d);

  &Decl::forceSingleDecl ($d);

  # Inlining contained subroutines called from parallel sections is mandatory
  # when they use some variables from the outer scope and these variables
  # are made FIRSTPRIVATE

  &Construct::changeIfStatementsInIfConstructs ($d);
  &Inline::inlineContainedSubroutines ($d);

  my $suffix = '_PARALLEL_SINGLE_COLUMN_FIELD_API';

  &wrapArrays ($d, $suffix);


  &Decl::declare ($d,  
                  'INTEGER (KIND=JPIM) :: IBL',
                  'INTEGER (KIND=JPIM) :: JLON',
                  'TYPE (CPG_BNDS_TYPE) :: YLCPG_BNDS');
  &Decl::use ($d,
              'USE ARRAY_MOD');

  &FieldAPI::pointers2FieldAPIPtr ($d);


  my @para = &F ('.//parallel-section', $d);

  for my $para (@para)
    {
      my ($stmt) = &F ('.//ANY-stmt', $para);
      my $indent = ' ' x &Fxtran::getIndent ($stmt);

      # Insert loop nest

      my $loop = "DO IBL = 1, YDCPG_OPTS%KGPBLKS\n";
      $loop .= "${indent}  CALL YDCPG_BNDS%UPDATE_VIEW (BLOCK_INDEX=IBL)\n";
 
      if ($opts{stack})
        {
          $loop .= "${indent}  ! Setup stack\n";
          $loop .= "${indent}  YLSTACK%L = LOC (YLSTACK%F_P%PTR (1,IBL))\n";
          $loop .= "${indent}  YLSTACK%U = YLSTACK%L + KIND (YLSTACK%F_P%PTR) * SIZE (YLSTACK%F_P%PTR (:,IBL))\n";
        }

      $loop .= "${indent}  DO JLON = YDCPG_BNDS%KIDIA, YDCPG_BNDS%KFDIA\n";
      $loop .= "${indent}    ! Select single column\n";
      $loop .= "${indent}    YLCPG_BNDS = YDCPG_BNDS\n";
      $loop .= "${indent}    YLCPG_BNDS%KIDIA = JLON\n";
      $loop .= "${indent}    YLCPG_BNDS%KFDIA = JLON\n";
      $loop .= "${indent}  ENDDO\n";
      $loop .= "${indent}ENDDO\n";

      my ($loop) = &Fxtran::fxtran (fragment => $loop);
      my ($loop_jlon) = &F ('./do-construct', $loop);

      my ($enddo) = &F ('.//end-do-stmt', $loop);
      my $p = $enddo->parentNode;
     
      for my $expr (&F ('.//named-E[string(N)="YDCPG_BNDS"]/N/n/text()', $para))
        {
          $expr->setData ('YLCPG_BNDS');
        }

      for my $node ($para->childNodes ()) 
        {   
          $p->insertBefore (&t (' ' x (4)), $enddo);
          &Fxtran::reIndent ($node, 4); 
          $p->insertBefore ($node, $enddo);
        }   
      $p->insertBefore (&t ($indent . '  '), $enddo);

      $para->appendChild (&t ("\n"));
      $para->appendChild ($loop);
      $para->insertBefore (&t ($indent), $loop);

      &Loop::removeJlonConstructs ($loop_jlon);
      &Loop::removeJlonLoopsFieldAPI ($d, $loop_jlon);

      &Call::addSuffix ($para, suffix => '_SINGLE_COLUMN_FIELD_API');

      # Insert OpenMP directive

      my @priv = &F ('.//a-stmt/E-1/named-E[not(.//component-R[string(ct)="PTR"])]/N|.//do-V/named-E/N', $para, 1);
      @priv = grep ({ $_ ne 'YLSTACK' } @priv) if ($opts{stack});

      my @first = ('YDCPG_BNDS');
      push @first, 'YLSTACK' if ($opts{stack});
      
      &OpenMP::parallelDo ($loop, PRIVATE => \@priv, FIRSTPRIVATE => \@first);

    }

  

  &Subroutine::addSuffix ($d, $suffix);


  return 1;
}

1;
