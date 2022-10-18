#!/home/gmap/mrpm/marguina/install/perl-5.32.0/bin/perl -w
#
use strict;
use FileHandle;
use Data::Dumper;
use File::Basename;

use FindBin qw ($Bin);
use lib $Bin;
use Fxtran;

&Fxtran::intfb ($_) for (@ARGV);
