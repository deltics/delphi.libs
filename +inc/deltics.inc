
{$ifdef VER80}  SORRY_BUT_DELPHI_1_IS_NOT_SUPPORTED {$endif}
{$ifdef VER90}  SORRY_BUT_DELPHI_2_IS_NOT_SUPPORTED {$endif}
{$ifdef VER100} SORRY_BUT_DELPHI_3_IS_NOT_SUPPORTED {$endif}
{$ifdef VER120} SORRY_BUT_DELPHI_4_IS_NOT_SUPPORTED {$endif}
{$ifdef VER130} SORRY_BUT_DELPHI_5_IS_NOT_SUPPORTED {$endif}
{$ifdef VER140} SORRY_BUT_DELPHI_6_IS_NOT_SUPPORTED {$endif}
{$ifdef VER160} SORRY_BUT_DELPHI_8_IS_NOT_SUPPORTED {$endif}


{$define UNKNOWN_COMPILER_VERSION}


{$ifdef VER150} // Delphi 7
  {$define DELPHI7}
  {$UNDEF UNKNOWN_COMPILER_VERSION}
{$endif}

{$ifdef VER170} // Delphi 2005
  {$define DELPHI2005}
  {$UNDEF UNKNOWN_COMPILER_VERSION}
{$endif}

{$ifdef VER180} // Delphi 2006
  {$define DELPHI2006}
  {$define DELPHI2006_OR_2007}
  {$UNDEF UNKNOWN_COMPILER_VERSION}
{$endif}

{$ifdef VER185} // Delphi 2007
  {$UNDEF DELPHI2006}
  {$define DELPHI2007}
  {$define DELPHI2006_OR_2007}
  {$UNDEF UNKNOWN_COMPILER_VERSION}
{$endif}

{$ifdef VER200} // RAD Studio 2009
  {$define DELPHI2009}
  {$UNDEF UNKNOWN_COMPILER_VERSION}
{$endif VER200}

{$ifdef VER210} // RAD Studio 2010
  {$define DELPHI2010}
  {$UNDEF UNKNOWN_COMPILER_VERSION}
{$endif VER210}

{$ifdef VER220} // RAD Studio XE
  {$define DELPHIXE}
  {$UNDEF UNKNOWN_COMPILER_VERSION}
{$endif VER220}

{$ifdef VER230} // RAD Studio XE2
  {$define DELPHIXE2}
  {$UNDEF UNKNOWN_COMPILER_VERSION}
{$endif VER230}

{$ifdef VER240} // RAD Studio XE3
  {$define DELPHIXE3}
  {$UNDEF UNKNOWN_COMPILER_VERSION}
{$endif VER240}

{$ifdef VER250} // RAD Studio XE4
  {$define DELPHIXE4}
  {$UNDEF UNKNOWN_COMPILER_VERSION}
{$endif VER250}

{$ifdef VER260} // RAD Studio XE5
  {$define DELPHIXE5}
  {$UNDEF UNKNOWN_COMPILER_VERSION}
{$endif VER260}


(*
  Template for adding support for new versions of Delphi as they come along.
   Don't forget to also update XX_OR_LATER and XX_OR_EARLIER section! (below)

{$ifdef VER NEW}
  {$define DELPHI NEW}
{$endif}
*)

{$ifdef UNKNOWN_COMPILER_VERSION}
  UNKNOWN_COMPILER_VERSION
{$endif}


{ ------------------------------------------------------------------------------------------------ }
{ DELPHInn_OR_LATER / EARLIER from DELPHInn_OR_LATER mappings                                      }
{ ------------------------------------------------------------------------------------------------ }

{.$define DELPHI NEW _OR_EARLIER}
{$define DELPHIXE5_OR_EARLIER}
{$define DELPHIXE4_OR_EARLIER}
{$define DELPHIXE3_OR_EARLIER}
{$define DELPHIXE2_OR_EARLIER}
{$define DELPHIXE_OR_EARLIER}
{$define DELPHI2010_OR_EARLIER}
{$define DELPHI2009_OR_EARLIER}
{$define DELPHI2007_OR_EARLIER}
{$define DELPHI2006_OR_EARLIER}
{$define DELPHI2005_OR_EARLIER}
{$define DELPHI7_OR_EARLIER}

(* {$ifdef DELPHI NEW}  {$define DELPHI NEW _OR_LATER} {$endif} *)
{$ifdef DELPHIXE5}  {$define DELPHIXE5_OR_LATER} {$endif}
{$ifdef DELPHIXE4}  {$define DELPHIXE4_OR_LATER} {$endif}
{$ifdef DELPHIXE3}  {$define DELPHIXE3_OR_LATER} {$endif}
{$ifdef DELPHIXE2}  {$define DELPHIXE2_OR_LATER} {$endif}
{$ifdef DELPHIXE}   {$define DELPHIXE_OR_LATER} {$endif}
{$ifdef DELPHI2010} {$define DELPHI2010_OR_LATER} {$endif}
{$ifdef DELPHI2009} {$define DELPHI2009_OR_LATER} {$endif}
{$ifdef DELPHI2007} {$define DELPHI2007_OR_LATER} {$endif}
{$ifdef DELPHI2006} {$define DELPHI2006_OR_LATER} {$endif}
{$ifdef DELPHI2005} {$define DELPHI2005_OR_LATER} {$endif}
{$ifdef DELPHI7}    {$define DELPHI7_OR_LATER} {$endif}

(* {$ifdef DELPHI NEW _OR_LATER}   {$define DELPHI PREV _OR_LATER}  {$undef DELPHI PREV _OR_EARLIER}  {$endif} *)
{$ifdef DELPHIXE5_OR_LATER}   {$define DELPHIXE4_OR_LATER}  {$undef DELPHIXE4_OR_EARLIER}  {$endif}
{$ifdef DELPHIXE4_OR_LATER}   {$define DELPHIXE3_OR_LATER}  {$undef DELPHIXE3_OR_EARLIER}  {$endif}
{$ifdef DELPHIXE3_OR_LATER}   {$define DELPHIXE2_OR_LATER}  {$undef DELPHIXE2_OR_EARLIER}  {$endif}
{$ifdef DELPHIXE2_OR_LATER}   {$define DELPHIXE_OR_LATER}   {$undef DELPHIXE_OR_EARLIER}   {$endif}
{$ifdef DELPHIXE_OR_LATER}    {$define DELPHI2010_OR_LATER} {$undef DELPHI2010_OR_EARLIER} {$endif}
{$ifdef DELPHI2010_OR_LATER}  {$define DELPHI2009_OR_LATER} {$undef DELPHI2009_OR_EARLIER} {$endif}
{$ifdef DELPHI2009_OR_LATER}  {$define DELPHI2007_OR_LATER} {$undef DELPHI2007_OR_EARLIER} {$endif}
{$ifdef DELPHI2007_OR_LATER}  {$define DELPHI2006_OR_LATER} {$undef DELPHI2006_OR_EARLIER} {$endif}
{$ifdef DELPHI2006_OR_LATER}  {$define DELPHI2005_OR_LATER} {$undef DELPHI2005_OR_EARLIER} {$endif}
{$ifdef DELPHI2005_OR_LATER}  {$define DELPHI7_OR_LATER}    {$undef DELPHI7_OR_EARLIER}    {$endif}


{ ------------------------------------------------------------------------------------------------ }
{ Some useful additional settings.                                                                 }
{ ------------------------------------------------------------------------------------------------ }

// Disable platform warnings since the Deltics libraries are (currently) Windows
//  specific and not intended to be used by C++ developers

{$WARN SYMBOL_PLATFORM      OFF}
{$WARN UNIT_PLATFORM        OFF}
{$WARN DUPLICATE_CTOR_DTOR  OFF}


// Scoped enums suck.  Making them the default sucks harder

{$ifdef DELPHIXE2_OR_LATER}
  {$scopedenums OFF}
{$endif}


{$ifdef DELPHI2009_OR_LATER}
  {$define UTF8_OVERLOADABLE}
{$endif}


// Who knew you could declare $APPTYPE anywhere !?
//  Now I can switch between CONSOLE and GUI with a project define
//  instead of having to put the APPTYPE declaration in the DPR!

{$ifdef CONSOLE}
  {$APPTYPE CONSOLE}
{$endif}



{ If INFESTED is defined then enable debugging of all Deltics library code ----------------------- }

{$ifdef INFESTED}

  {$define deltics_bonjour}
  {$define deltics_rtl}
  {$define deltics_smoketest}

{$endif}
