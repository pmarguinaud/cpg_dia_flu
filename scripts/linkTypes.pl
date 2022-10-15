#!/home/gmap/mrpm/marguina/install/perl-5.32.0/bin/perl -w
#
use strict;
use FileHandle;
use Data::Dumper;
use File::Basename;
use Storable;

use FindBin qw ($Bin);
use lib $Bin;

my %T;

for my $f (<types/*.pl>)
  {
    my $T = &basename ($f, qw (.pl));
    $T{$T} = do ("./$f");
  }


for my $T (keys (%T))
  {
    if (my $super = $T{$T}{super})
      {
        $T{$T}{super} = $T{$super};
      }
    for (values (%{ $T{$T}{comp} }))
      {
        if (ref ($_))
          {
            my $t = $$_;
            $_ = $T{$t};
          }
      }
  }

my %TT;

while (my ($t, $h) = each (%T))
  {
    my %h;
    for (my $hh = $h; $hh; $hh = $hh->{super})
      {
        while (my ($k, $v) = each (%{ $hh->{comp} }))
          {
            if (ref ($v) && ref ($v) eq 'HASH')
              {
                $TT{$v->{name}} ||= {};
                $h{$k} = $TT{$v->{name}};
              }
            else
              {
                $h{$k} = $v;
              }
          }
      }
    $TT{$t} ||= {};
    %{ $TT{$t} } = %h;
  }

print &Dumper (\%TT);

&Storable::nstore (\%TT, 'types.dat');
