#!/usr/bin/perl -w

use strict;

use FileHandle;
use File::Copy;
use File::Basename;
use File::stat;
use File::Path;
use Getopt::Long;
use FindBin qw ($Bin);
use lib $Bin;

use Common;

use Fxtran;

my %opts;

sub newer
{
  my ($f1, $f2)  = @_;
  die unless (-f $f1);
  return 1 unless (-f $f2);
  return stat ($f1)->mtime > stat ($f2)->mtime;
}

sub copyIfNewer
{
  my ($f1, $f2) = @_;

  if (&newer ($f1, $f2))
    {
      print "Copy $f1 to $f2\n"; 
      &copy ($f1, $f2); 
    }
}

sub saveToFile
{
  my ($x, $f) = @_;

  unless (-d (my $d = &dirname ($f)))
    {
      &mkpath ($d);
    }

  'FileHandle'->new (">$f")->print ($x->textContent ());
  'FileHandle'->new (">$f.xml")->print ($x->toString ());
}

sub apply
{
  my ($method, $doc, $file) = @_;
  my ($module, $routine) = split (m/::/o, $method);

  eval "use $module;";

  my $c = $@; 
  $c && die ($c);

  {
    no strict 'refs';
    &{$method} ($doc);
  }

  &saveToFile ($doc, "tmp/$routine/$file");
}

sub saveSubroutine
{
  my $d = shift;
  my ($F90) = &F ('./object/file/program-unit/subroutine-stmt/subroutine-N/N/n/text()', $d, 1);
  $F90 = lc ($F90) . '.F90';
  'FileHandle'->new (">$F90")->print ($d->textContent);
  &Fxtran::intfb ($F90);
  return $F90;
}

sub generateSyncHost
{
  my $f = shift;
  my $d = &Fxtran::fxtran (location => $f, fopts => [qw (-line-length 500)]);

  &FieldAPI::makeSyncHost ($d);

  &saveSubroutine ($d);
}

sub generateParallelFieldAPI
{
  use Parallel;
  use Stack;
  use Decl;

  my $f = shift;
  my $d = &Fxtran::fxtran (location => $f, fopts => [qw (-line-length 500)]);

  return unless (&F ('.//C[starts-with(string(.),"!=PARALLEL")', $d));

  &Parallel::makeParallelFieldAPI ($d);
  &Decl::changeIntent ($d, 'YDCPG_BNDS', 'INOUT');

  &saveSubroutine ($d);
}

sub generateParallelSingleColumnFieldAPI
{
  use Parallel;
  use Stack;
  use Decl;

  my $f = shift;
  my $d = &Fxtran::fxtran (location => $f, fopts => [qw (-line-length 500)]);

  return unless (&F ('.//C[starts-with(string(.),"!=PARALLEL")', $d));

  &Parallel::makeParallelSingleColumnFieldAPI ($d, stack => 1);
  &Decl::changeIntent ($d, 'YDCPG_BNDS', 'INOUT');
  &Stack::addStack ($d, skip => sub { my ($proc, $call) = @_; return $proc =~ m/SYNC_HOST/o; });

  &saveSubroutine ($d);
}

sub generateFieldAPI
{
  use Subroutine;
  use Call;

  my $f = shift;
  my $d = &Fxtran::fxtran (location => $f, fopts => [qw (-line-length 500)]);

  &FieldAPI::pointers2FieldAPIPtr ($d);

  &Subroutine::addSuffix ($d, '_FIELD_API');
  &Call::addSuffix ($d, suffix => '_FIELD_API');

  &saveSubroutine ($d);
}

sub generateParallelView
{
  use Parallel;
  use Stack;

  my $f = shift;
  my $d = &Fxtran::fxtran (location => $f, fopts => [qw (-line-length 500)]);

  return unless (&F ('.//C[starts-with(string(.),"!=PARALLEL")', $d));

  &Parallel::makeParallelView ($d);

  &saveSubroutine ($d);
}

sub generateSingleColumnFieldAPI
{
  use Subroutine;
  use Call;
  use Decl;
  use Stack;

  my $f = shift;
  my $d = &Fxtran::fxtran (location => $f, fopts => [qw (-line-length 500)]);

  my @method = qw (
    Associate::resolveAssociates
    Construct::changeIfStatementsInIfConstructs
    Inline::inlineContainedSubroutines
    FieldAPI::pointers2FieldAPIPtr
    Loop::removeJlonLoops
  );

  for my $method (@method)
    {
      &apply ($method, $d, $f);
    }

  my $suffix = '_SINGLE_COLUMN_FIELD_API';

  &Call::addSuffix ($d, suffix => $suffix);
  &Subroutine::addSuffix ($d, $suffix);
  &Stack::addStack ($d);

  &saveSubroutine ($d);
}

sub preProcessIfNewer
{
  my ($f1, $f2, $opts) = @_;

  if (&newer ($f1, $f2))
    {
      use Associate;
      use Inline;
      use Construct;
      use DrHook;

      print "Preprocess $f1\n";

      &copy ($f1, $f2);

      &Fxtran::intfb ($f2);

      &generateSyncHost ($f1);

      &generateParallelView ($f1);

      &generateParallelFieldAPI ($f1);

      &generateSingleColumnFieldAPI ($f1);

      &generateFieldAPI ($f1);

      &generateParallelSingleColumnFieldAPI ($f1);
    }
}

my @opts_f = qw (update compile external-drhook);
my @opts_s = qw (arch);

&GetOptions
(
  map ({ ($_,     \$opts{$_}) } @opts_f),
  map ({ ("$_=s", \$opts{$_}) } @opts_s),
);


my @compute = map { &basename ($_) } <compute/*.F90>;
my @support = grep { ! m/\.F90\.xml$/o } map { &basename ($_) } <support/*>;

if ($opts{'external-drhook'})
  {
    @support = grep { $_ !~ m/^(?:abor1|yomhook)\.F90$/o } @support; 
  }

my $dir = "compile.$opts{arch}";

&mkpath ($dir);
chdir ($dir);

if ($opts{update})
  {
    for my $f (@support)
      {
        &copyIfNewer ("../support/$f", $f);
      }
    
    if ($opts{arch} =~ m/^gpu/o)
      {
        for my $f (@compute)
          {
            &preProcessIfNewer ("../compute/$f", $f);
          }
      }
   else
     {
        for my $f (@compute)
          {
#           &copyIfNewer ("../compute/$f", $f);
            &preProcessIfNewer ("../compute/$f", $f, \%opts);
          }
     }

    &copy ("../Makefile.$opts{arch}", "Makefile.inc");

    system ("$Bin/Makefile.PL") and die;
  }

if ($opts{compile})
  {
    system ('make -j4 wrap_cpg_dia_flux.x') and die;
  }





