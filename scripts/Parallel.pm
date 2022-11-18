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
use OpenACC;
use Call;
use Associate;
use Directive;
use Loop;


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


sub makeBlockViewSection
{
  my $d = shift;
  my %args = @_;
  my $para = $args{section};
  my $updatable = $args{updatable};

  my @N = &uniq (grep { $updatable->{$_} } &F ('.//named-E/N/n/text()',  $para, 1));

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

  my @priv = grep { ! $updatable->{$_} } &F ('.//a-stmt/E-1/named-E/N|.//do-V/named-E/N', $para, 1);
  
  $para->insertBefore (&t ("\n"), $loop);
  &OpenMP::parallelDo ($loop, PRIVATE => \@priv, FIRSTPRIVATE => [sort keys (%$updatable)]);
}


sub getUpdatables
{

# Look for updatable objects; as a side effect

  my $d = shift;

  my (%T, %updatable);

  for my $en_decl (&F ('.//EN-decl', $d))
    {
      my ($stmt) = &Fxtran::stmt ($en_decl);
      next unless (my ($ts) = &F ('./_T-spec_/derived-T-spec', $stmt));
      my ($N) = &F ('./EN-N', $en_decl, 1);
      my ($T) = &F ('./T-N', $ts, 1);
      $T{$N} = $T;
      $updatable{$N} = &FieldAPI::isUpdatable ($T);
    }

  $updatable{YDCPG_BNDS} = 1;

  return \%updatable;
}

sub makeUpdatablesInout
{
  my $d = shift;
  my $updatable = shift;

  for my $U (sort keys (%$updatable))
    {
      unless ($updatable->{$U})
        {
          delete $updatable->{$U};
          next;
        }
      next unless (my ($decl) = &F ('.//T-decl-stmt[.//EN-N[string(.)="?"]', $U, $d));
      next unless (my ($intent) = &F ('./attribute/intent-spec/text()', $decl));
      $intent->setData ('INOUT');
    }
}

sub makeBlockFieldAPISection
{
  my $d = shift;
  my %args = @_;
  my $para = $args{section};

  my $what = $para->getAttribute ('target') || 'host';
  
  &FieldAPI::pointers2FieldAPIPtr ($d, what => $what, section => $para);
  
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
  
  &Call::addSuffix ($d, section => $para, suffix => '_FIELD_API_' . uc ($what));
  
  # Insert OpenMP directive
  
  my @priv = &F ('.//a-stmt/E-1/named-E[not(.//component-R[string(ct)="PTR"])]/N|.//do-V/named-E/N', $para, 1);
  @priv = grep { $_ ne 'YDCPG_BNDS' } @priv;
  
  &OpenMP::parallelDo ($loop, PRIVATE => \@priv, FIRSTPRIVATE => ['YDCPG_BNDS']);
}

sub makePostSyncSection
{
  my $d = shift;
  my ($para, $outline, $call, $include) = @_;

  my $rename_device_to_host = sub
  {
    my $text = shift;
    my $tt = $text->data;
    $tt =~ s{(_DEVICE)}{my $host = "_HOST"; lc ($1) eq $1 ? lc ($host) : uc ($host)}eio;
    $text->setData ($tt);
  };

  my $tt;

  # Sync routine

  $outline = $outline->cloneNode (1);

  for (&F ('.//call-stmt/procedure-designator/named-E/N/n/text()[contains(string(.),"SYNC_")]', $outline),
       &F ('.//call-stmt/procedure-designator/named-E/R-LT/component-R/ct[contains(string(.),"SYNC_")]/text()', $outline),
       &F ('.//include/filename/text()[contains(string(.),"_sync")]', $outline))
    {
      $rename_device_to_host->($_);
    }

  $rename_device_to_host->(&F ('./object/file/program-unit/subroutine-stmt/subroutine-N/N/n/text()', $outline));

  &saveFileWithSubroutineName ($outline);

  # Call statement

  $call = $call->cloneNode (1);

  $rename_device_to_host->(&F ('./procedure-designator/named-E/N/n/text()', $call));

  $para->parentNode->insertAfter ($call, $para);
  $para->parentNode->insertAfter (&t ("\n"), $para) for (1 .. 2);

  # Include
  
  my $include1 = $include->cloneNode (1);
  $include->parentNode->insertAfter ($include1, $include);
  $include->parentNode->insertAfter (&t ("\n"), $include);

  $rename_device_to_host->(&F ('./filename/text()', $include1));
}

sub saveFileWithSubroutineName
{
  my $body = shift;
  my ($name) = &F ('./object/file/program-unit/subroutine-stmt/subroutine-N/N/n/text()', $body, 1);
  'FileHandle'->new ('>' . lc ($name) . ".F90")->print ($body->textContent);
  return $name;
}

sub makeSyncSection
{
  my $d = shift;
  my %args = @_;
  my $para = $args{section};

  my ($name) = &F ('.//subroutine-N', $d, 1);
  my $i = $args{number};

  my $what = $para->getAttribute ('target') || 'host';
  
  my $para1 = $para->cloneNode (1);
  $para->parentNode->insertBefore ($para1, $para);
  $para->parentNode->insertBefore (&t ("\n"), $para);
  use Outline;
  my $oc = &Outline::outlineSection ($d, section => $para1, name => "$name\_PARALLEL_$i");
  my ($outline, $call, $include) = @$oc;
  
  &FieldAPI::makeSync ($outline, what => $what);
  
  $name = &saveFileWithSubroutineName ($outline);
  
  my ($filename) = &F ('./filename/text()', $include);
  $filename->setData (lc ($name) . '.intfb.h');
  
  my ($proc) = &F ('./procedure-designator/named-E/N/n/text()', $call);
  $proc->setData ($name);
  
  &Fxtran::fold ($call);

  my $post = lc ($para->getAttribute ('post') || '');

  if ($post eq 'synchost')
    {
      &makePostSyncSection ($d, $para, $outline, $call, $include);
    }
  elsif ($post)
    {
      die $post;
    }

}

sub makeSingleColumnFieldAPISection
{
  my $d = shift;
  my %args = @_;
  my $para = $args{section};

  my $what = $para->getAttribute ('target') || 'host';
  
  &FieldAPI::pointers2FieldAPIPtr ($d, what => $what, section => $para);
  
  my $PTR = $what eq 'host' ? 'PTR' : 'DEVPTR';

  my ($stmt) = &F ('.//ANY-stmt', $para);
  my $indent = ' ' x &Fxtran::getIndent ($stmt);
  
  # Insert loop nest
  
  my $loop = "DO IBL = 1, YDCPG_OPTS%KGPBLKS\n";

  if ($args{inline_update_bnds})
    {
      $loop .= "${indent}  YDCPG_BNDS%KBL    = IBL\n";
      $loop .= "${indent}  YDCPG_BNDS%KSTGLO = 1 + (IBL - 1) * YDCPG_BNDS%KLON\n";
      $loop .= "${indent}  YDCPG_BNDS%KFDIA  = MIN (YDCPG_BNDS%KLON, YDCPG_BNDS%KGPCOMP - YDCPG_BNDS%KSTGLO + 1)\n";
    }
  else
    {
      $loop .= "${indent}  CALL YDCPG_BNDS%UPDATE_VIEW (BLOCK_INDEX=IBL)\n";
    }
  
  if ($args{stack})
    {
      $loop .= "${indent}  ! Setup stack\n";
      $loop .= "${indent}  YDSTACK%L = LOC (YDSTACK%F_P%$PTR (1,IBL))\n";
      $loop .= "${indent}  YDSTACK%U = YDSTACK%L + KIND (YDSTACK%F_P%$PTR) * SIZE (YDSTACK%F_P%$PTR (:,IBL))\n";
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
  
  &Call::addSuffix ($d, section => $para, suffix => '_SINGLE_COLUMN_FIELD_API_' . uc ($what));
  
  # Insert OpenMP directive
  
  my $PTR = $what eq 'host' ? 'PTR' : 'DEVPTR';

  my $directive = lc ($para->getAttribute ('directive') || 'openmp');

  if ($directive eq 'openmp')
    {
      my @priv = &F ('.//a-stmt/E-1/named-E[not(.//component-R[string(ct)="?"])]/N|.//do-V/named-E/N', $PTR, $para, 1);
      @priv = grep ({ $_ ne 'YDSTACK' } @priv) if ($args{stack});
      my @first = ('YDCPG_BNDS');
      push @first, 'YDSTACK' if ($args{stack});
      my %first = map { ($_, 1) } @first;
      @priv = grep { ! $first{$_} } @priv;
  
      &OpenMP::parallelDo ($loop, PRIVATE => \@priv, FIRSTPRIVATE => \@first);
    }
  elsif ($directive eq 'openacc')
    {
      &OpenACC::parallelLoopGang ($loop, PRIVATE => ['IBL'], FIRSTPRIVATE => ['YDCPG_BNDS', $args{stack} ? ('YDSTACK') : ()]);

      my ($loop_vector) = &F ('./do-construct', $loop);
      my @priv = &F ('.//a-stmt/E-1/named-E[not(.//component-R[string(ct)="?"])]/N|.//do-V/named-E/N', $PTR, $loop_vector, 1);
  
      &OpenACC::loopVector ($loop_vector, PRIVATE => \@priv, FIRSTPRIVATE => []);
    }
  else
    {
      die $directive;
    }
}

sub makeParallel
{
  my $d = shift;
  my %args = @_;
  my $suffix = $args{suffix};

  # Resolving ASSOCIATEs in parallel sections is mandatory
  &Associate::resolveAssociates ($d);

  &Decl::forceSingleDecl ($d);

  # Inlining contained subroutines called from parallel sections is mandatory
  # when they use some variables from the outer scope and these variables
  # are made FIRSTPRIVATE

  &Construct::changeIfStatementsInIfConstructs ($d);
  &Inline::inlineContainedSubroutines ($d);

  &wrapArrays ($d, $suffix);

  my @para = &F ('.//parallel-section', $d);

  my $singlecolumn = grep { my $vector = lc ($_->getAttribute ('vector') || 'block'); $vector eq 'singlecolumn' } @para;

  &Decl::declare ($d,  
                  'INTEGER (KIND=JPIM) :: IBL',
                   $singlecolumn ? ('INTEGER (KIND=JPIM) :: JLON', 'TYPE (CPG_BNDS_TYPE) :: YLCPG_BNDS') : ());

  my @array = &uniq (&F ('.//T-decl-stmt/_T-spec_/derived-T-spec/T-N[starts-with(string(.),"ARRAY_")]', $d, 1));
  &Decl::use ($d,
              'USE ARRAY_MOD, ONLY : ' . join (', ', @array));

  my $updatable = &getUpdatables ($d);
  &makeUpdatablesInout ($d, $updatable);

  my $i = 0;
  for my $para (@para)
    {
      &makeSyncSection ($d, %args, section => $para, number => $i++);
      my $vector = lc ($para->getAttribute ('vector') || 'block');
      my $datalayout = lc ($para->getAttribute ('datalayout') || 'fieldapi');

      if ($datalayout eq 'fieldapi')
        {
          if ($vector eq 'singlecolumn')
            {
              &makeSingleColumnFieldAPISection ($d, %args, section => $para, stack => 1, inline_update_bnds => 1);
            }
          elsif ($vector eq 'block')
            {
              &makeBlockFieldAPISection ($d, %args, section => $para);
            }
          else
            {
              die $vector;
            }
        }
      elsif ($datalayout eq 'view')
        {
          if ($vector eq 'block')
            {
              &makeBlockViewSection ($d, section => $para, updatable => $updatable);
            }
          else
            {
              die $vector;
            }
        }
    }

  &Subroutine::addSuffix ($d, $suffix);

  return 1;
}

1;
