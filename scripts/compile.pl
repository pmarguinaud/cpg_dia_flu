#!/home/gmap/mrpm/marguina/install/perl-5.32.0/bin/perl -w

use strict;
use FindBin qw ($Bin);
use lib $Bin;
use FileHandle;
use File::Copy;
use File::Basename;
use File::stat;
use File::Path;
use Getopt::Long;
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

sub preProcessIfNewer
{
  my ($f1, $f2) = @_;

  if (&newer ($f1, $f2))
    {
      use Associate;
      use Inline;
      use Construct;

      print "Preprocess $f1\n";

      my $d = &Fxtran::fxtran (location => $f1, fopts => [qw (-line-length 500)]);
      &saveToFile ($d, "tmp/$f2");

      &Associate::resolveAssociates ($d);
      &saveToFile ($d, "tmp/resolveAssociates/$f2");

      &Construct::changeIfStatementsInIfConstructs ($d);
      &saveToFile ($d, "tmp/changeIfStatementsInIfConstructs/$f2");

      &Inline::inlineContainedSubroutines ($d);
      &saveToFile ($d, "tmp/inlineContainedSubroutines/$f2");

      'FileHandle'->new (">$f2")->print ($d->textContent ());

      &Fxtran::intfb ($f2);
    }
}

my @opts_f = qw (update compile);
my @opts_s = qw (arch);

&GetOptions
(
  map ({ ($_,     \$opts{$_}) } @opts_f),
  map ({ ("$_=s", \$opts{$_}) } @opts_s),
);

my @compute = map { &basename ($_) } <compute/*.F90>;
my @support = grep { ! m/\.F90\.xml$/o } map { &basename ($_) } <support/*>;

&mkpath ("compile.$opts{arch}");

chdir ("compile.$opts{arch}");

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
            &preProcessIfNewer ("../compute/$f", $f);
          }
     }

    &copy ("../Makefile.$opts{arch}", "Makefile.inc");

    system ("$Bin/Makefile.PL") and die;
  }

if ($opts{compile})
  {
    system ('make -j4 wrap_cpg_dia_flux.x') and die;
  }





