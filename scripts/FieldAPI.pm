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

  use FindBin qw ($Bin);
  my $TI = &Storable::retrieve ("$Bin/types.dat");
  
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


sub fieldify
{
  my $d = shift;

  my @en_decl = (&F ('.//EN-decl[./array-spec/shape-spec-LT/shape-spec[string(upper-bound)="YDCPG_OPTS%KLON"]]', $d),
                 &F ('.//EN-decl[./array-spec/shape-spec-LT/shape-spec[string(upper-bound)="KLON"]]', $d));

  my @NPROMA = map { &F ('./EN-N', $_, 1) } @en_decl;

  my %LOCAL;

  # Convert NPROMA arrays into field API objects

  for my $en_decl (@en_decl)
    {
      my ($N) = &F ('./EN-N', $en_decl, 1);
      my ($stmt) = &Fxtran::stmt ($en_decl);
      my ($as) = &F ('./array-spec', $en_decl);
      $as->unbindNode ();
      my ($ts) = &F ('./_T-spec_/node()', $stmt);
      
      if (my ($intent) = &F ('./attribute[string(attribute-N)="INTENT"]', $stmt))
        {
          $ts->replaceNode (&n ('<derived-T-spec>CLASS(<T-N><N><n>FIELD_BASIC</n></N></T-N>)</derived-T-spec>'));
        }
      else
        {
          my @ss = &F ('./shape-spec-LT/shape-spec', $as);
          my $d = scalar (@ss) + 1;
          $ts->replaceNode (&n ("<derived-T-spec>CLASS(<T-N><N><n>FIELD_BASIC</n></N></T-N>)</derived-T-spec>"));
          # Set to NULL
          $en_decl->appendChild (&t (' => '));
          $en_decl->appendChild (&e ('NULL ()'));
          $LOCAL{$N} = 1;
        }

      my ($dd) = &F ('./text()[contains(.,"::")]', $stmt);
      $stmt->insertBefore (&t (', '), $dd);
      $stmt->insertBefore (&n ('<attribute><attribute-N>POINTER</attribute-N></attribute>'), $dd);
    }

  # Remove array reference of NPROMA arrays

  for my $NPROMA (@NPROMA)
    {
      my @expr = &F ('.//named-E[string(N)="?"]', $NPROMA, $d);
      for my $expr (@expr)
        {
          my ($rlt) = &F ('./R-LT', $expr);
          $rlt && $rlt->unbindNode ();
        }
    }


  use FindBin qw ($Bin);
  my $TI = &Storable::retrieve ("$Bin/types.dat");
  my @T = sort keys (%$TI);

  # Find objects containing field API pointers and map them to their types

  my %T;
  for my $T (@T)
    {
      my @F = &F ('.//T-decl-stmt[./_T-spec_/derived-T-spec[string(T-N)="?"]]//EN-decl/EN-N/N/n/text()', $T, $d, 1);
      for my $F (@F)
        {
          $T{$F} = $T;
        }
    }
  my @F = sort keys (%T);

  # This anonymous sub tests whether and expression contains NPROMA data (field API or local/argument array)
  my $isNproma = sub
  {
    my $expr = shift;
    my ($N) = &F ('./N', $expr, 1);
    return 1 if (grep { $N eq $_ } @NPROMA);
    return 0 unless (my $T = $T{$N});
    my @ct = &F ('./R-LT/component-R/ct', $expr);
    my $fd = &getFieldAPIMember ($TI, $T, map ({ $_->textContent } @ct));
    return $fd;
  };
  

  # Handle call statements

  my @call = &F ('.//call-stmt', $d);
  my %proc;

  for my $call (@call)
    {
      my ($proc) = &F ('./procedure-designator/named-E/N/n/text()', $call);
      my @expr = &F ('./arg-spec/arg/named-E', $call);
      for my $expr (@expr)
        {
          next unless ($isNproma->($expr));
          push @{ $proc{$proc->textContent} }, $proc;
          last;
        }
    }

  while (my ($name, $list) = each (%proc))
    {
      for my $proc (@$list)
        {
          $proc->setData ($name . '_PLAN');
        }
      next unless (my ($filename) = &F ('.//include/filename[string(.)="?"]/text()', lc ($name) . '.intfb.h', $d));
      $filename->setData (lc ($name) . '_plan.intfb.h');
    }


  # Remove if construct where conditions involve NPROMA expressions (local arrays of field API pointers)

  for my $ifc (&F ('.//if-construct', $d))
    {
      my $found = 0;
      for my $expr (&F ('./if-block/ANY-stmt/condition-E//named-E', $ifc))
        {
          if ($isNproma->($expr))
            {
              $found = 1;
              last;
            }
        }

      next unless ($found);
      my @block = &F ('./if-block', $ifc);
      for (@block)
        {
          $_->firstChild->unbindNode;
        }
      $block[-1]->lastChild->unbindNode ();
      for my $node (&F ('./if-block/node()', $ifc))
        {
          $ifc->parentNode->insertBefore ($node, $ifc);
        }
      $ifc->unbindNode ();
    }

  # Find where field data is modified

  for my $stmt (&F ('.//a-stmt', $d))
    {
      my $indent = ' ' x &Fxtran::getIndent ($stmt);
      my $n = 0;
      for my $expr (&F ('./E-1//named-E', $stmt))
        {
          next unless ($isNproma->($expr));
          my $dum = &s ($expr->textContent . " = ZDUM");
          $stmt->parentNode->insertBefore ($dum, $stmt);
          $stmt->parentNode->insertBefore (&t ("\n$indent"), $stmt);
          $n++;
        }
      for my $expr (&F ('./E-2//named-E', $stmt))
        {
          next unless ($isNproma->($expr));
          my $dum = &s ("ZDUM = " . $expr->textContent);
          $stmt->parentNode->insertBefore ($dum, $stmt);
          $stmt->parentNode->insertBefore (&t ("\n$indent"), $stmt);
          $n++;
        }
      $n && $stmt->unbindNode ();
    }
 

  # Use the field API object

  my %YLFLDPTR;

  for my $F (@F)
    {
      my $T = $T{$F};
      my @expr = &F ('.//named-E[string(N)="?"][R-LT]', $F, $d);
      for my $expr (@expr)
        {

          my @ct = &F ('./R-LT/component-R/ct', $expr);
          my $fd = &getFieldAPIMember ($TI, $T, map ({ $_->textContent } @ct));
          next unless ($fd);
  
          # Remove last reference if it is an array reference
          my @r = &F ('./R-LT/ANY-R', $expr);
          my $ar = $r[-1];
          if (($ar->nodeName eq 'parens-R') || ($ar->nodeName eq 'array-R'))
            {
              $ar && $ar->unbindNode ();
            }
          my ($f, $d) = @$fd;
          $ct[-1]->replaceNode (my $ct = &n ("<ct>$f</ct>"));

          my $stmt = &Fxtran::stmt ($expr);

          if ($stmt->nodeName eq 'call-stmt')
            {
              my $target = $expr->cloneNode (1);
              my %basic = map { m/^YLFLDPTR/o ? ($_, 1) : () } &F ('./arg-spec/arg/named-E/N', $stmt, 1);
              my $basic;
              for (my $i = 0; ; $i++)
                {
                  $basic = "YLFLDPTR" . $i;
                  last unless ($basic{$basic});
                }
              $YLFLDPTR{$basic}++;
              $expr->replaceNode (&e ($basic));
              my $assoc = &s ("$basic => " . $target->textContent);
              my $indent = "\n" . (' ' x &Fxtran::getIndent ($stmt));
              $stmt->parentNode->insertBefore ($assoc, $stmt);
              $stmt->parentNode->insertBefore (&t ($indent), $stmt);
            }
          
        }
    }
 
  my ($drhook) = &F ('.//if-stmt[.//call-stmt[string(procedure-designator)="DR_HOOK"]]', $d);
  my $indent = "\n" . (' ' x &Fxtran::getIndent ($drhook));

  for my $var (sort keys (%YLFLDPTR))
    {
      $drhook->parentNode->insertBefore (&s ("CLASS (FIELD_BASIC), POINTER :: $var"), $drhook);
      $drhook->parentNode->insertBefore (&t ($indent), $drhook);
    }

  # Remove loops on JLEV or JLON

  for my $ind (qw (JLON JLEV))
   {
     my @do = &F ('.//do-construct[./do-stmt[string(do-V)="?"]]', $ind, $d);
     
     for my $do (@do)
       {   
         $do->firstChild->unbindNode;
         $do->lastChild->unbindNode;
 
         # Remove calculations involving non-NPROMA data

         for (&F ('.//a-stmt[string(E-1)!="ZDUM" and string(E-2)!="ZDUM"]', $do))
           {
             $_->unbindNode ();
           }

         # Remove construct

         my @nodes = &F ('./node()', $do);
         for (@nodes)
           {
             $do->parentNode->insertBefore ($_, $do);
           }
         $do->unbindNode (); 
       }   
   }

  $d->normalize ();

  my @stmt = &F ('.//ANY-stmt', $d);

  my (@g, @G);

  for my $i (0 .. $#stmt)
    {
      my $stmt = $stmt[$i];
      if (($stmt->nodeName eq 'a-stmt') && (&F ('./E-1[string(.)="ZDUM"]|./E-2[string(.)="ZDUM"]', $stmt)))
        {
          push @g, $stmt;
        }
      elsif (@g)
        {
          push @G, [@g];
          @g = ();
        }
    }

  for my $g (@G)
    {
      my (%rd, %rw);
      for my $stmt (@$g)
        {
          (my ($E1) = &F ('./E-1', $stmt, 1)) =~ s/\s+//go;
          (my ($E2) = &F ('./E-2', $stmt, 1)) =~ s/\s+//go;

          $rw{$E1}++ unless ($E1 eq 'ZDUM');
          $rd{$E2}++ unless ($E2 eq 'ZDUM');

        }
      
      delete $rd{$_} for (keys (%rw));

      for (keys (%LOCAL))
        {
          delete $rd{$_};
          delete $rw{$_};
        }
      
      my ($stmt) = @$g;
      my $indent = "\n" . (' ' x &Fxtran::getIndent ($stmt));

      for (sort keys (%rw))
        {
          my $if = m/%/o ? "" : "IF (ASSOCIATED ($_)) ";
          $stmt->parentNode->insertBefore (&s ("${if}CALL $_%SYNC_HOST_RDWR"), $stmt);
          $stmt->parentNode->insertBefore (&t ($indent), $stmt);
        }

      for (sort keys (%rd))
        {
          my $if = m/%/o ? "" : "IF (ASSOCIATED ($_)) ";
          $stmt->parentNode->insertBefore (&s ("${if}CALL $_%SYNC_HOST_RDONLY"), $stmt);
          $stmt->parentNode->insertBefore (&t ($indent), $stmt);
        }

      for my $stmt (reverse @$g)
        {
          my $prev = $stmt->previousSibling;
          $prev->unbindNode ();
          $stmt->unbindNode ();
        }
    }

  
  my @sn = &F ('./object/file/program-unit/subroutine-stmt/subroutine-N/N/n/text()|./object/file/program-unit/end-subroutine-stmt/subroutine-N/N/n/text()', $d);

  for my $sn (@sn) 
    {
      $sn->setData ($sn->data . '_PLAN');
    }


  unless (my ($use) = &F ('.//use-stmt[string(use-N)="FIELD_MODULE"][./rename-LT/rename/use-N[string(.)="FIELD_BASIC"]]', $d))
    {
      ($use) = &F ('.//use-stmt', $d);
      $use->parentNode->insertBefore (&s ("USE FIELD_MODULE, ONLY : FIELD_BASIC"), $use);
      $use->parentNode->insertBefore (&t ("\n"), $use);
    }

    
  my @drhook_name = &F ('.//call-stmt[string(procedure-designator)="DR_HOOK"]/arg-spec/arg/string-E/S/text()', $d);

  for (@drhook_name)
    {
      (my $str = $_->data) =~ s/(["'])$/_PLAN$1/go;
      $_->setData ($str);
    }



}


1;
