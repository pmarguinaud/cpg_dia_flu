#!/usr/bin/perl -w

use strict;

use Data::Dumper;
use Getopt::Long;
use File::Path;
use File::Spec;
use FindBin qw ($Bin);
use lib $Bin;

use Common;

use Fxtran;


sub process_decl
{
  my ($opts, $en_decl, $sname, $prefix, 
      $BODY_SAVE, $BODY_LOAD, $BODY_COPY, $BODY_WIPE, $BODY_SIZE, 
      $U, $J, $L, $B, $en_decl_hash) = @_;

  my (@BODY_SAVE, @BODY_LOAD, @BODY_COPY, @BODY_WIPE, @BODY_SIZE);
  my (%U, %J, %L, %B);

  my $stmt = &Fxtran::stmt ($en_decl);

  return if ($stmt->nodeName eq 'final-stmt');

  my %attr = map { ($_, 1) } &F ('.//attribute/attribute-N/text()', $stmt);

  return if ($attr{PARAMETER});

  my ($name) = &F ('.//EN-N/N/n/text()', $en_decl, 1);
  
  my $skip = $opts->{'skip-components'}->($sname, $name, \%attr, $en_decl_hash);

  if ($skip)
    {
      if ($attr{POINTER})
        {
          push @BODY_LOAD, "NULLIFY ($prefix$name)";
        }
      goto RETURN;
    }
  
  my ($tspec) = &F ('./_T-spec_', $stmt);

  die $stmt->toString unless ($tspec);

  return if (uc ($tspec->textContent) =~ m/^PROCEDURE/o);

  my ($intrinsic) = &F ('./intrinsic-T-spec', $tspec);
  my ($tname) = &F ('./derived-T-spec/T-N/N/n/text()', $tspec);
  
  $tname && ($U{$tname} = 1);
  
  
  my @ss = &F ('./array-spec/shape-spec-LT/shape-spec', $en_decl, 1);

  unless (@ss)
    {
      my $stmt = &Fxtran::stmt ($en_decl);
      @ss = &F ('.//attribute/array-spec/shape-spec-LT/shape-spec', $stmt, 1);
    }
  
  
  if ($attr{POINTER} || $attr{ALLOCATABLE})
    {
      my $func = $attr{POINTER} ? 'ASSOCIATED' : 'ALLOCATED';
      push @BODY_SAVE, "L$name = $func ($prefix$name)\n";
      push @BODY_COPY, "L$name = $func ($prefix$name)\n";
      push @BODY_WIPE, "L$name = $func ($prefix$name)\n";
      push @BODY_SIZE, "L$name = $func ($prefix$name)\n";
      push @BODY_SAVE, "WRITE (KLUN) L$name\n";
      push @BODY_LOAD, "READ (KLUN) L$name\n";
      $L{$name} = 1;
      push @BODY_SAVE, "IF (L$name) THEN\n";
      push @BODY_LOAD, "IF (L$name) THEN\n";
      push @BODY_COPY, "IF (L$name) THEN\n";
      push @BODY_WIPE, "IF (L$name) THEN\n";
      push @BODY_SIZE, "IF (L$name) THEN\n";
      if (@ss)
        {
          push @BODY_SAVE, "WRITE (KLUN) LBOUND ($prefix$name)\n";
          push @BODY_SAVE, "WRITE (KLUN) UBOUND ($prefix$name)\n";
          $B{scalar (@ss)} = 1;
          my $r = scalar (@ss);
          push @BODY_LOAD, "READ (KLUN) IL" . scalar (@ss) . "\n";
          push @BODY_LOAD, "READ (KLUN) IU" . scalar (@ss) . "\n";
          push @BODY_LOAD, "ALLOCATE ($prefix$name (" . join (', ', map { "IL$r($_):IU$r($_)" } (1 .. $#ss+1) ) . "))\n";
        }
      else
        {
          push @BODY_LOAD, "ALLOCATE ($prefix$name)\n" unless ($tname && grep { $_ eq $tname } @{ $opts->{'no-allocate'} });
        }
      push @BODY_COPY, "!\$acc enter data create ($prefix$name)\n";
      push @BODY_COPY, "!\$acc update device ($prefix$name)\n";
      push @BODY_WIPE, "!\$acc exit data detach ($prefix$name)\n";
    }
  

  if ($intrinsic)
    {
      my ($tn) = &F ('./T-N', $intrinsic, 1);
      push @BODY_SAVE, "WRITE (KLUN) $prefix$name\n";
      push @BODY_LOAD, "READ (KLUN) $prefix$name\n";
      my $size = "ISIZE = KIND ($prefix$name)"; 
      $size .= " * SIZE ($prefix$name)" if (@ss);
      $size .= " * LEN ($prefix$name)" if ($tn eq 'CHARACTER');
      push @BODY_SIZE, $size . "\n", 
                       "IF (LDPRINT) THEN\n", 
                       "WRITE (*, '(I10,\" \")', ADVANCE='NO') ISIZE\n", 
                       "WRITE (*, *) TRIM (CDPATH)//'%$name'\n", 
                       "ENDIF\n", 
                       "KSIZE = KSIZE + ISIZE\n";
    }
  else 
    {
      push @BODY_SIZE, "JSIZE = 0\n";
      for (my $i = $#ss+1; $i >= 1; $i--)
        {
          $J{"J$i"} = 1;
          my $do = "DO J$i = LBOUND ($prefix$name, $i), UBOUND ($prefix$name, $i)\n";
          push @BODY_SAVE, $do;
          push @BODY_LOAD, $do;
          push @BODY_COPY, $do;
          push @BODY_WIPE, $do;
          push @BODY_SIZE, $do;
        }
      my @J = map { "J$_"  } (1 .. $#ss+1);
      my $J = @ss ? " (" . join (', ', @J) . ")" : '';
#     my $LDPRINT = @J ? ".FALSE." : "LDPRINT";
      my $LDPRINT = '.FALSE.';
      push @BODY_SAVE, ('  ' x scalar (@ss)) 
                   . "CALL SAVE (KLUN, $prefix$name" . $J . ")\n";
      push @BODY_LOAD, ('  ' x scalar (@ss)) 
                   . "CALL LOAD (KLUN, $prefix$name" . $J . ")\n";
      push @BODY_COPY, ('  ' x scalar (@ss)) 
                   . "CALL COPY ($prefix$name" . $J . ", LDCREATED=.TRUE.)\n";
      push @BODY_WIPE, ('  ' x scalar (@ss)) 
                   . "CALL WIPE ($prefix$name" . $J . ", LDDELETED=.TRUE.)\n";
      push @BODY_SIZE, ('  ' x scalar (@ss))
                   . "ISIZE = SIZE ($prefix$name" . $J . ", CDPATH//'%$name', $LDPRINT)\n", 
                     "JSIZE = JSIZE + ISIZE\n",
                     "KSIZE = KSIZE + ISIZE\n";
      for (my $i = $#ss; $i >= 0; $i--)
        {
          push @BODY_SAVE, "ENDDO\n";
          push @BODY_LOAD, "ENDDO\n";
          push @BODY_COPY, "ENDDO\n";
          push @BODY_WIPE, "ENDDO\n";
          push @BODY_SIZE, "ENDDO\n";
        }
      push @BODY_SIZE, 
                     "IF (LDPRINT) THEN\n", 
                     "WRITE (*, '(I10,\" \")', ADVANCE='NO') JSIZE\n",
                     "WRITE (*, *) TRIM (CDPATH)//'%$name'\n",
                     "ENDIF\n", 
    }
  
  if ($attr{POINTER} || $attr{ALLOCATABLE})
    {
      push @BODY_SAVE, "ENDIF\n";
      if ($attr{POINTER})
        {
          push @BODY_LOAD, "ELSE\n", "NULLIFY ($prefix$name)\n";
        }
      push @BODY_LOAD, "ENDIF\n";
      push @BODY_COPY, "!\$acc enter data attach ($prefix$name)\n";
      push @BODY_COPY, "ENDIF\n";
      push @BODY_WIPE, "!\$acc exit data delete ($prefix$name)\n";
      push @BODY_WIPE, "ENDIF\n";
      push @BODY_SIZE, "ENDIF\n";
    }
  push @BODY_COPY, "\n";
  push @BODY_WIPE, "\n";

RETURN:
  
  push @$BODY_SAVE, @BODY_SAVE;
  push @$BODY_LOAD, @BODY_LOAD;
  push @$BODY_COPY, @BODY_COPY;
  push @$BODY_WIPE, @BODY_WIPE;
  push @$BODY_SIZE, @BODY_SIZE;

  %$U = (%$U, %U); %$J = (%$J, %J); 
  %$L = (%$L, %L); %$B = (%$B, %B); 

}

sub indent
{
  my $n = 0;
  for (@_)
    {
      chomp;
      s/^\s*//o;
      $n-- if (m/^\s*(?:ELSE|ENDIF|ENDDO)\b/o);
      $_ = ('  ' x $n) . $_;
      $n++ if (m/^\s*(?:ELSE|IF|DO)\b/o);
    }
}

sub r
{
  my $f = shift;
  return '' unless (-f $f);
  return do { local $/ = undef; my $fh = 'FileHandle'->new ("<$f"); <$fh> };
}

sub w
{
  my $f = shift;
  my $t = &r ($f);
  return if ($t eq $_[0]);
  'FileHandle'->new (">$f")->print ($_[0]);
}

sub process_types
{
  my ($doc, $opts) = @_;

  my %code;
  my @file;

  my ($mod) = &F ('.//module-stmt/module-N/N/n/text()', $doc);
  
  my @tconst = &F ('.//T-construct', $doc);
  
  for my $tconst (@tconst)
    {
      my ($name) = &F ('.//T-stmt/T-N/N/n/text()', $tconst, 1);
      my $tname = $name;
  
      next if ($opts->{'skip-types'}->($name));

      my ($abstract) = &F ('./T-stmt/attribute[string(attribute-N)="ABSTRACT"]', $tconst);
      my ($extends) = &F ('./T-stmt/attribute[string(attribute-N)="EXTENDS"]/N/n/text()', $tconst);

      my ($INTERFACE_SAVE, $CONTAINS_SAVE) = ('', '');
      my ($INTERFACE_LOAD, $CONTAINS_LOAD) = ('', '');
      my ($INTERFACE_COPY, $CONTAINS_COPY) = ('', '');
      my ($INTERFACE_WIPE, $CONTAINS_WIPE) = ('', '');
      my ($INTERFACE_SIZE, $CONTAINS_SIZE) = ('', '');
  
  
      $INTERFACE_SAVE .= "MODULE PROCEDURE SAVE_$name\n";
      $INTERFACE_LOAD .= "MODULE PROCEDURE LOAD_$name\n";
      $INTERFACE_COPY .= "MODULE PROCEDURE COPY_$name\n";
      $INTERFACE_WIPE .= "MODULE PROCEDURE WIPE_$name\n";
      $INTERFACE_SIZE .= "MODULE PROCEDURE SIZE_$name\n";
  
      my (@BODY_SAVE, @BODY_LOAD, @BODY_COPY, @BODY_WIPE, @BODY_SIZE);

      push @BODY_COPY, "LLCREATED = .FALSE.\n",
                       "IF (PRESENT (LDCREATED)) THEN\n",
                       "LLCREATED = LDCREATED\n",
                       "ENDIF\n",
                       "IF (.NOT. LLCREATED) THEN\n",
                       "!\$acc enter data create (YD)\n",
                       "!\$acc update device (YD)\n",
                       "ENDIF\n";
      push @BODY_SIZE, 'KSIZE = 0';

      if ($extends)
        {
          for (\@BODY_SAVE, \@BODY_LOAD, \@BODY_COPY, \@BODY_WIPE, \@BODY_SIZE)
            {
              push @$_, "YLSUPER => YD\n";
            }
          push @BODY_SAVE, "CALL SAVE_$extends (KLUN, YLSUPER)\n";
          push @BODY_LOAD, "CALL LOAD_$extends (KLUN, YLSUPER)\n";
          push @BODY_COPY, "CALL COPY_$extends (YLSUPER, LDCREATED=.TRUE.)\n";
          push @BODY_WIPE, "CALL WIPE_$extends (YLSUPER, LDDELETED=.TRUE.)\n";
          push @BODY_SIZE, "KSIZE = KSIZE + SIZE_$extends (YLSUPER, CDPATH, LDPRINT)\n";
        }
    
      my (%U, %J, %L, %B);

      my @en_decl = &F ('.//EN-decl', $tconst);
      my %en_decl;
      for my $en_decl (@en_decl)
        {
          my ($name) = &F ('.//EN-N/N/n/text()', $en_decl, 1);
          $en_decl{$name} = $en_decl;
        }
      for my $en_decl (@en_decl)
        {
          &process_decl ($opts, $en_decl, "$tname%", 'YD%', 
                         \@BODY_SAVE, \@BODY_LOAD, \@BODY_COPY, \@BODY_WIPE, \@BODY_SIZE, 
                         \%U, \%J, \%L, \%B, \%en_decl);
        }

      push @BODY_WIPE, "LLDELETED = .FALSE.\n",
                       "IF (PRESENT (LDDELETED)) THEN\n", 
                       "LLDELETED = LDDELETED\n",
                       "ENDIF",
                       "IF (.NOT. LLDELETED) THEN\n",
                       "!\$acc exit data delete (YD)\n",
                       "ENDIF\n";
                       
  
      my $DECL_SAVE = '';
      my $DECL_LOAD = '';
      my $DECL_COPY = "LOGICAL :: LLCREATED\n";
      my $DECL_WIPE = "LOGICAL :: LLDELETED\n";
      my $DECL_SIZE = "INTEGER*8 :: ISIZE, JSIZE\n";

      if ($extends)
        {
          for ($DECL_SAVE, $DECL_LOAD, $DECL_COPY, $DECL_WIPE, $DECL_SIZE)
            {
              $_ .= "CLASS ($extends), POINTER :: YLSUPER\n";
            }
        }
  
      if (%J)
        {
          $DECL_SAVE .= "INTEGER :: " . join (', ', sort keys (%J)) . "\n";
          $DECL_LOAD .= "INTEGER :: " . join (', ', sort keys (%J)) . "\n";
          $DECL_COPY .= "INTEGER :: " . join (', ', sort keys (%J)) . "\n";
          $DECL_WIPE .= "INTEGER :: " . join (', ', sort keys (%J)) . "\n";
          $DECL_SIZE .= "INTEGER :: " . join (', ', sort keys (%J)) . "\n";
        }
      if (%B)
        {
          $DECL_LOAD .= "INTEGER :: " . join (', ', map  { ("IL$_($_)", "IU$_($_)") } sort keys (%B)) . "\n";
        }
      if (%L)
        {
          my @L = sort keys (%L);
          while (my @l = splice (@L, 0, 10))
            {
              $DECL_SAVE .= "LOGICAL :: " . join (', ', map { "L$_" } @l) . "\n";
              $DECL_LOAD .= "LOGICAL :: " . join (', ', map { "L$_" } @l) . "\n";
              $DECL_COPY .= "LOGICAL :: " . join (', ', map { "L$_" } @l) . "\n";
              $DECL_WIPE .= "LOGICAL :: " . join (', ', map { "L$_" } @l) . "\n";
              $DECL_SIZE .= "LOGICAL :: " . join (', ', map { "L$_" } @l) . "\n";
            }
        }
  
      my @U = map { "UTIL_${_}_MOD" } sort keys (%U);
      @U = map { (exists ($opts->{'module-map'}{$_}) ? $opts->{'module-map'}{$_} : $_) } @U;
      %U = map { ($_, 1) } @U;
      @U = sort keys (%U);

      my $USE = join ('', map { "USE ${_}\n" } grep { $_ ne $name } @U);

      if ($extends)
        {
          $USE .= "USE UTIL_${extends}_MOD, ONLY : $extends, SAVE_$extends\n" if ($opts->{save});
          $USE .= "USE UTIL_${extends}_MOD, ONLY : $extends, LOAD_$extends\n" if ($opts->{load});
          $USE .= "USE UTIL_${extends}_MOD, ONLY : $extends, COPY_$extends\n" if ($opts->{copy});
          $USE .= "USE UTIL_${extends}_MOD, ONLY : $extends, WIPE_$extends\n" if ($opts->{wipe});
          $USE .= "USE UTIL_${extends}_MOD, ONLY : $extends, SIZE_$extends\n" if ($opts->{size});
        }

      for ($USE)
        {
          chomp ($_);
        }
  
      my $type = $abstract ? 'CLASS' : 'TYPE';
#     my $type = 'CLASS';

      $CONTAINS_SAVE .= << "EOF";
SUBROUTINE SAVE_$name (KLUN, YD)
IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
$type ($name), INTENT (IN), TARGET :: YD
EOF

      $CONTAINS_LOAD .= << "EOF";
SUBROUTINE LOAD_$name (KLUN, YD)
IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
$type ($name), INTENT (OUT), TARGET :: YD
EOF

      $CONTAINS_COPY .= << "EOF";
SUBROUTINE COPY_$name (YD, LDCREATED)
IMPLICIT NONE
$type ($name), INTENT (IN), TARGET :: YD
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED
EOF

      $CONTAINS_WIPE .= << "EOF";
SUBROUTINE WIPE_$name (YD, LDDELETED)
IMPLICIT NONE
$type ($name), INTENT (IN), TARGET :: YD
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED
EOF

      $CONTAINS_SIZE .= << "EOF";
INTEGER*8 FUNCTION SIZE_$name (YD, CDPATH, LDPRINT) RESULT (KSIZE)
IMPLICIT NONE
$type ($name),     INTENT (IN), TARGET :: YD
CHARACTER(LEN=*), INTENT (IN) :: CDPATH
LOGICAL,          INTENT (IN) :: LDPRINT
EOF

      &indent (@BODY_SAVE);
      &indent (@BODY_LOAD);
      &indent (@BODY_COPY);
      &indent (@BODY_WIPE);
      &indent (@BODY_SIZE);


      $CONTAINS_SAVE .= $DECL_SAVE . "\n" . join ("\n", @BODY_SAVE, '') . "END SUBROUTINE\n";
      $CONTAINS_LOAD .= $DECL_LOAD . "\n" . join ("\n", @BODY_LOAD, '') . "END SUBROUTINE\n";
      $CONTAINS_COPY .= $DECL_COPY . "\n" . join ("\n", @BODY_COPY, '') . "END SUBROUTINE\n";
      $CONTAINS_WIPE .= $DECL_WIPE . "\n" . join ("\n", @BODY_WIPE, '') . "END SUBROUTINE\n";
      $CONTAINS_SIZE .= $DECL_SIZE . "\n" . join ("\n", @BODY_SIZE, '') . "END FUNCTION\n";

      for ($CONTAINS_SAVE, $CONTAINS_SAVE, $CONTAINS_COPY, $CONTAINS_WIPE, $CONTAINS_SIZE, $INTERFACE_SAVE, $INTERFACE_LOAD, $INTERFACE_COPY, $INTERFACE_WIPE, $INTERFACE_SIZE)
        {
          chomp ($_);
        }
  
      my $n = lc ($name);

      $CONTAINS_SAVE = '' unless ($opts->{save});
      $CONTAINS_LOAD = '' unless ($opts->{load});
      $CONTAINS_COPY = '' unless ($opts->{copy});
      $CONTAINS_WIPE = '' unless ($opts->{wipe});
      $CONTAINS_SIZE = '' unless ($opts->{size});

      $INTERFACE_SAVE = "INTERFACE SAVE\n$INTERFACE_SAVE\nEND INTERFACE\n";
      $INTERFACE_LOAD = "INTERFACE LOAD\n$INTERFACE_LOAD\nEND INTERFACE\n";
      $INTERFACE_COPY = "INTERFACE COPY\n$INTERFACE_COPY\nEND INTERFACE\n";
      $INTERFACE_WIPE = "INTERFACE WIPE\n$INTERFACE_WIPE\nEND INTERFACE\n";
      $INTERFACE_SIZE = "INTERFACE SIZE\n$INTERFACE_SIZE\nEND INTERFACE\n";

      if ($abstract)
        {
          $INTERFACE_SAVE = "";
          $INTERFACE_LOAD = "";
          $INTERFACE_COPY = "";
          $INTERFACE_WIPE = "";
          $INTERFACE_SIZE = "";
        }
  
      $INTERFACE_SAVE = '' unless ($opts->{save});
      $INTERFACE_LOAD = '' unless ($opts->{load});
      $INTERFACE_COPY = '' unless ($opts->{copy});
      $INTERFACE_WIPE = '' unless ($opts->{wipe});
      $INTERFACE_SIZE = '' unless ($opts->{size});

      push @file, "util_${n}_mod.F90";

      $code{"util_${n}_mod.F90"} = << "EOF";
MODULE UTIL_${name}_MOD

USE $mod, ONLY : $name
$USE

$INTERFACE_SAVE
$INTERFACE_LOAD
$INTERFACE_COPY
$INTERFACE_WIPE
$INTERFACE_SIZE

CONTAINS

$CONTAINS_SAVE

$CONTAINS_LOAD

$CONTAINS_COPY

$CONTAINS_WIPE

$CONTAINS_SIZE

END MODULE
EOF

    }

  if ($opts->{out})
    {
      &w ("$opts->{dir}/$opts->{out}", join ('', map { $code{$_} } @file));
    }
  else
    {
      for my $file (@file)
        {
          &w ("$opts->{dir}/$file", $code{$file});
        }
    }

}

sub registerFieldAPI
{
  my ($tc, $opts, $class) = @_;

  my %h;

  my ($type) = &F ('.//T-stmt/T-N/N/n/text()', $tc, 1);

  my ($abstract) = &F ('./T-stmt/attribute[string(attribute-N)="ABSTRACT"]', $tc);
  my ($extends) = &F ('./T-stmt/attribute[string(attribute-N)="EXTENDS"]/N/n/text()', $tc);

  my @en_decl = &F ('.//EN-decl', $tc);
  my %en_decl;

  for my $en_decl (@en_decl)
    {
      my ($name) = &F ('.//EN-N/N/n/text()', $en_decl, 1);
      $en_decl{$name} = $en_decl;
    }

  for my $en_decl (@en_decl)
    {
      my ($name) = &F ('.//EN-N/N/n/text()', $en_decl, 1);
      my ($stmt) = &Fxtran::stmt ($en_decl);
      my %attr = map { ($_, 1) } &F ('.//attribute/attribute-N/text()', $stmt);

      my ($tspec) = &F ('./_T-spec_', $stmt);
      if (my ($tname) = &F ('./derived-T-spec/T-N/N/n/text()', $tspec, 1))
        {
          $h{$name} = \$tname unless ($tname =~ m/^FIELD_/o);
        }
      elsif ($class && (my $fam = $class->getFieldAPIMember ($type, $name, \%attr, \%en_decl)))
        {
          my @ss = &F ('./array-spec/shape-spec-LT/shape-spec', $en_decl);
          $h{$name} = [$fam, scalar (@ss)];
        }
    }


  my $update_view = 0;
  if (&F ('./procedure-stmt/procedure-N-LT/rename[string(use-N)="UPDATE_VIEW"]', $tc))
    {
      $update_view = 1;
    }

  return {comp => \%h, name => $type, super => ($extends && $extends->textContent), update_view => $update_view};
}

my %opts = qw (dir .);

&GetOptions
(
  'skip-components=s' => \$opts{'skip-components'}, 'skip-types=s' => \$opts{'skip-types'},
  'only-components=s' => \$opts{'only-components'}, 'only-types=s' => \$opts{'only-types'},
  'dir=s' => \$opts{dir}, 'out=s' => \$opts{out},
  size => \$opts{size}, save => \$opts{save}, load => \$opts{load}, copy => \$opts{copy}, wipe => \$opts{wipe},
  'no-allocate=s' => \$opts{'no-allocate'}, 'module-map=s' => \$opts{'module-map'},
  'field-api' => \$opts{'field-api'}, 'field-api-class=s' => \$opts{'field-api-class'},
  'tmp=s' => \$opts{tmp},
);

( -d $opts{dir}) or &mkpath ($opts{dir});

if (! $opts{'no-allocate'})
  {
    $opts{'no-allocate'} = [];
  }
else
  {
    $opts{'no-allocate'} = [split (m/,/o, $opts{'no-allocate'})];
  }

if (! $opts{'module-map'})
  {
    $opts{'module-map'} = {};
  }
else
  {
    $opts{'module-map'} = {split (m/,/o, $opts{'module-map'})};
  }

sub parseListOrCodeRef
{
  my ($opts, $kw) = @_;

  if (-f "$Bin/$opts->{$kw}.pm")
    {
      my $class = $opts->{$kw};
      eval "use $class;";
      my $c = $@;
      $c && die ($c);
      $opts->{$kw} = sub { $class->skip (@_) };
    }
  elsif ($opts->{$kw} =~ m/^sub /o)
    {
      $opts->{$kw} = eval ($opts->{$kw});
      my $c = $@;
      die $c if ($c);
    }
  elsif ($kw =~ m/-components$/o)
    {
      my @comp = split (m/,/o, $opts->{$kw});
      if ($kw =~ m/^skip-/o)
        {
          $opts->{$kw} = sub { my ($type, $comp) = @_; grep { $_ eq "$type$comp" } @comp };
        }
      else
        {
          $opts->{$kw} = sub { my ($type, $comp) = @_; grep { $_ eq "$type$comp" } @comp };
        }
    }
  elsif ($kw =~ m/-types$/o)
    {
      my @type = split (m/,/o, $opts->{$kw});
      if ($kw =~ m/^skip-/o)
        {
          $opts->{$kw} = sub { my ($type) = @_; grep { $_ eq "$type" } @type };
        }
      else
        {
          $opts->{$kw} = sub { my ($type) = @_; grep { $_ eq "$type" } @type };
        }
    }
}

sub parseSkipOnly
{
  my ($opts, $skip, $only) = @_;
  if ($opts->{$skip})
    {
      &parseListOrCodeRef ($opts, $skip);
    }
  elsif ($opts->{$only})
    {
      &parseListOrCodeRef ($opts, $only);
      $opts->{$skip} = sub { ! $opts->{$only}->(@_) };
    }
  else
    {
      $opts->{$skip} = sub { 0 };
    }
}

&parseSkipOnly (\%opts, 'skip-components', 'only-components');
&parseSkipOnly (\%opts, 'skip-types', 'only-types');

my $F90 = shift;

my $doc = &Fxtran::fxtran (location => $F90, fopts => [qw (-line-length 800)], dir => $opts{tmp});

if ($opts{load} || $opts{save} || $opts{size} || $opts{copy} || $opts{wipe})
  {
    &process_types ($doc, \%opts);
  }

if ($opts{'field-api'})
  {
    my $class;

    if ($class = $opts{'field-api-class'})
      {
        eval "use $class";
        my $c = $@;
        $c && die ($c);
      }

    my @tc = &F ('.//T-construct', $doc);
    
    for my $tc (@tc)
      {
        my ($type) = &F ('.//T-stmt/T-N/N/n/text()', $tc, 1);
        next if ($opts{'skip-types'}->($type));
        my $h = &registerFieldAPI ($tc, \%opts, $class);
        local $Data::Dumper::Sortkeys = 1;
        'FileHandle'->new (">types/$type.pl")->print (&Dumper ($h));
    }
  }
