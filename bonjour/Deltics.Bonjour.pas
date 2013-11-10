{
  * X11 (MIT) LICENSE *

  Copyright © 2008 Jolyon Smith

  Permission is hereby granted, free of charge, to any person obtaining a copy of
   this software and associated documentation files (the "Software"), to deal in
   the Software without restriction, including without limitation the rights to
   use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is furnished to do
   so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.


  * GPL and Other Licenses *

  The FSF deem this license to be compatible with version 3 of the GPL.
   Compatability with other licenses should be verified by reference to those
   other license terms.


  * Contact Details *

  Original author : Jolyon Smith
  skype           : deltics
  e-mail          : <EXTLINK mailto: jsmith@deltics.co.nz>jsmith@deltics.co.nz</EXTLINK>
  website         : <EXTLINK http://www.deltics.co.nz>www.deltics.co.nz</EXTLINK>
}

{$i deltics.bonjour.inc}

{$ifdef deltics_bonjour_component}
  {$debuginfo ON}
{$else}
  {$debuginfo OFF}
{$endif}

  unit Deltics.Bonjour;


interface

  uses
  { vcl: }
    Classes,
    Contnrs,
    SysUtils,
  { deltics: }
    Deltics.Bonjour.API;


  type
    TBonjourComponent = class;
    EBonjour = class;


    TDomainType   = (
                     dtBrowsing,
                     dtRegistration
                    );

    TProtocol     = (
                     bpTCP,
                     bpUDP
                    );


    TBonjourComponent = class(TComponent)
    private
      fDesignActive: Boolean;
//      fProtocol: TProtocol;
      procedure set_Active(const aValue: Boolean);
    protected
      function get_Active: Boolean; virtual;
      function get_Handle: TDNSServiceRef; virtual;
      procedure Activate; virtual; abstract;
      procedure Deactivate; virtual;
      procedure EnsureNotActive;
      procedure Loaded; override;
    public
      procedure BeforeDestruction; override;
      property Handle: TDNSServiceRef read get_Handle;
    public
      property Active: Boolean read get_Active write set_Active;
//      property Protocol: TProtocol read fProtocol write fProtocol;
    end;


    EBonjour = class(Exception)
      constructor Create(const aError: TDNSServiceErrorType); reintroduce; overload;
    end;


  procedure CheckResult(const aResult: TDNSServiceErrorType);


implementation

  uses
  { vcl: }
    Messages,
    SyncObjs,
    Windows,
  { deltics: }
    Deltics.Bonjour.Thread;


{ TBonjourComponent }

  procedure TBonjourComponent.BeforeDestruction;
  begin
    set_Active(FALSE);

    inherited;
  end;


  function TBonjourComponent.get_Active: Boolean;
  begin
    if (csDesigning in ComponentState) then
      result := fDesignActive
    else
      result := (Handle <> NIL);
  end;


  function TBonjourComponent.get_Handle: TDNSServiceRef;
  begin
    result := NIL;
  end;


  procedure TBonjourComponent.set_Active(const aValue: Boolean);
  begin
    if (Active = aValue) then
      EXIT;

    if (ComponentState * [csDesigning, csLoading] <> []) then
      fDesignActive := aValue
    else
      case aValue of
        FALSE : Deactivate;
        TRUE  : Activate;
      end;
  end;


  procedure TBonjourComponent.Deactivate;
  begin
    // NO-OP
  end;


  procedure TBonjourComponent.EnsureNotActive;
  begin
    if Active then
      raise Exception.Create('Operation not possible whilst component has an active '
                           + 'connection to the DNS-SD runtime');
  end;



  procedure CheckResult(const aResult: TDNSServiceErrorType);

    function ReturnAddr: Pointer; asm MOV EAX,[EBP+4] end;

  begin
    if (aResult <> kDNSServiceErr_NoError) then
      raise EBonjour.Create(aResult) at ReturnAddr;
  end;





  procedure TBonjourComponent.Loaded;
  begin
    inherited;

    if NOT (csDesigning in ComponentState) then
      Active := fDesignActive;
  end;





{ EBonjourError }

  constructor EBonjour.Create(const aError: TDNSServiceErrorType);
  begin
    inherited CreateFmt('Bonjour API error %d', [aError]);
  end;




end.
