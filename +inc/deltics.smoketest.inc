
{$i deltics.inc}


{$ifdef deltics_smoketest}

  {$define deltics_smoketest_console}
  {$define deltics_smoketest_expectations}
  {$define deltics_smoketest_filewriter}

{$endif}


{$ifdef DELPHI2009_OR_LATER}
  {$define EnhancedOverloads}
{$endif}

