sub
{
  use strict;
  my ($type, $comp, $attr, $en_decl_hash) = @_;
  
  return unless ($attr->{POINTER});

  return 1 if ($comp =~ m/^(?:ZVIEW|F_DATA|ZDATA)$/o);


  if (my $en_decl = $en_decl_hash->{"F_$comp"})
    {
      my $stmt = &Fxtran::stmt ($en_decl);
      my ($tspec) = &Fxtran::F ('./_T-spec_', $stmt);  
      my ($tname) = &F ('./derived-T-spec/T-N/N/n/text()', $tspec);
      return 1 if ($tname =~ m/^FIELD_/o);
    }

  return 0;
}
